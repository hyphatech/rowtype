(* Migrations on Postgres: rowtype-migrate's record over Postgres's own
   ways -- an advisory lock over the run, a transaction known by its id,
   [reset all] after each, and pg_dump for the schema. *)

module S = Rowtype
module M = Rowtype_migrate
module Pg = Rowtype_postgres

let ( let* ) = Result.bind
let log = Logs.Src.create "rowtype-migrate.postgres"

module Log = (val Logs.src_log log)

module Postgres_dialect = struct
  (* The advisory lock a run takes is Postgres's alone, so it travels with
     Postgres's connection rather than through the record's rules. *)
  type conn = { pg : Pg.conn; lock : int }

  let run conn statement args = Pg.run conn.pg statement args
  let exec_raw conn sql = Pg.exec_raw conn.pg sql

  let within conn work =
    Pg.Transaction.within conn.pg (fun pg -> work { conn with pg })

  let backend = S.find ~params:S.unit ~row:S.int "select pg_backend_pid()"

  (* The advisory lock is the session's, and ends with its backend; a
     transaction makes a lost connection again before it begins, on a new
     backend that holds no lock. So a migration runs only on the backend
     that took the lock, asked first thing in its own transaction, where a
     [revive] has already happened if it was going to. *)
  let holding ~holder conn =
    let* pid = run conn backend () in
    if pid = holder then Ok () else Error (`Lock `Lost)

  (* A migration is bounded by nothing but itself, and so is the wait for
     another process's: a connection may carry a statement timeout and a
     bound on its reads meant for requests, either of which would kill a
     long migration half-way. [reset] gives back the statement bound the
     connection was opened with, and the read bound is put back as it was,
     whatever the run ends in. A migration's failure is the answer over the
     release's, which a lost connection has already made. *)
  let exclusively conn f =
    let reads = Pg.timeout conn.pg in
    Pg.set_timeout conn.pg ~timeout:None;
    Fun.protect ~finally:(fun () -> Pg.set_timeout conn.pg ~timeout:reads)
    @@ fun () ->
    let* () = exec_raw conn "set statement_timeout = 0" in
    let* () =
      run conn (S.exec ~params:S.int "select pg_advisory_lock($1)") conn.lock
    in
    let outcome =
      let* holder = run conn backend () in
      f ~holding:(holding ~holder)
    in
    let released =
      let* () =
        run conn
          (S.exec ~params:S.int "select pg_advisory_unlock($1)")
          conn.lock
      in
      exec_raw conn "reset statement_timeout"
    in
    let* () = outcome in
    released

  (* The table, made with every column it has today, and a table made
     before brought up to it: the checksum, whose rows' sums are recorded on
     first sight, and the source, whose rows are the application's -- the
     only history there was -- and which joins the version in the key, since
     a version is unique within its source. Made under the run's lock, so
     two processes never both bring one table up. *)
  let prepare_record ~table conn =
    let* () =
      exec_raw conn
        (Printf.sprintf
           {|create table if not exists %s (
               source text not null default '%s',
               version bigint not null,
               name text not null,
               applied_at timestamptz not null default now(),
               checksum text,
               primary key (source, version));
             alter table %s
               add column if not exists checksum text;
             alter table %s
               add column if not exists source text not null default '%s'|}
           table M.app table table M.app)
    in
    let* key =
      run conn
        (S.find_opt ~params:S.text ~row:(S.t2 S.text S.int)
           "select conname::text, cardinality(conkey) from pg_constraint where \
            conrelid = to_regclass($1) and contype = 'p'")
        table
    in
    match key with
    | Some (name, 1) ->
        exec_raw conn
          (Printf.sprintf
             "alter table %s drop constraint \"%s\", add primary key (source, \
              version)"
             table name)
    | Some _ | None -> Ok ()

  (* Found as the statements that write it find it, through the search
     path, whether or not its name says a schema. *)
  let record_columns ~table conn =
    let* exists =
      run conn
        (S.find_opt ~params:S.text ~row:S.bool
           "select to_regclass($1) is not null")
        table
    in
    let has column =
      run conn
        (S.find_opt ~params:(S.t2 S.text S.text) ~row:S.bool
           "select exists (select 1 from pg_attribute where attrelid = \
            to_regclass($1) and attname = $2 and not attisdropped)")
        (table, column)
      |> Result.map (Option.value ~default:false)
    in
    match exists with
    | Some true ->
        let* checksum = has "checksum" in
        let* source = has "source" in
        Ok (M.Present { checksum; source })
    | Some false | None -> Ok M.Absent

  (* A transaction is known by its id, so a file that begins another after
     its end is found too. *)
  type mark = int

  let mark conn =
    run conn (S.find ~params:S.unit ~row:S.int "select txid_current()") ()

  let unbroken conn id =
    Result.map
      (Option.equal Int.equal (Some id))
      (run conn
         (S.find ~params:S.unit ~row:(S.opt S.int)
            "select txid_current_if_assigned()")
         ())

  (* A [set] outlives the transaction it ran in -- a pg_dump, which a
     baseline is, sets an empty search path -- so it is put back, with the
     run's own unbounded statements. *)
  let reset conn = exec_raw conn "reset all; set statement_timeout = 0"
end

module On_postgres = M.Make (Postgres_dialect)

(* Any fixed number, the same in every process that migrates one database:
   it is what makes two of them take turns. *)
let default_lock = 7_265_756_171

let apply ?(lock = default_lock) ?(table = M.default_table) ?(libraries = [])
    conn migrations =
  On_postgres.apply ~table ~libraries { pg = conn; lock } migrations

let status ?(table = M.default_table) ?(libraries = []) conn migrations =
  (* A status reads, under no lock; the connection carries one regardless. *)
  On_postgres.status ~table ~libraries
    { pg = conn; lock = default_lock }
    migrations

(* ------------------------------------------------------------------ *)
(* A database *)

(* A connect is bounded, since a server that never answers is otherwise a
   command that never ends; what runs on the connection is not, the
   driver's bound on every read lifted once it is made, since an
   administrator's statement -- a database made or dropped, a migration --
   runs as long as it takes. *)
let connect_timeout = Mtime.Span.(10 * s)

(* Quiet: [create table if not exists] is a NOTICE on every run, and a notice
   is a line in what a command prints. *)
let connect ~sw ~net ~mono_clock (c : Pg.Conninfo.t) =
  let c =
    Pg.Conninfo.with_connect_timeout c
      (Option.value c.connect_timeout ~default:connect_timeout)
  in
  let* conn =
    Pg.connect ~sw ~net ~mono_clock
      ~parameters:[ ("client_min_messages", "warning") ]
      c
  in
  Pg.set_timeout conn ~timeout:None;
  Ok conn

(* A finaliser runs in a fiber that may be cancelled, where any IO raises
   again: shielded, it does its work, and the cancellation goes on as itself
   rather than wrapped in [Fun.Finally_raised]. *)
let closing conn f =
  Fun.protect
    ~finally:(fun () -> Eio.Cancel.protect (fun () -> Pg.close conn))
    (fun () -> f conn)

(* A database that is not there is the one failure to connect with a next
   step to name, and the server's own database says whether it is that. *)
let missing ~sw ~net ~mono_clock (c : Pg.Conninfo.t) e =
  match
    let* admin =
      connect ~sw ~net ~mono_clock (Pg.Conninfo.with_database c "postgres")
    in
    closing admin (fun admin ->
        Result.map
          (fun there -> (c.database, there))
          (Pg.run admin
             (S.find ~params:S.text ~row:S.bool
                "select exists (select 1 from pg_database where datname = $1)")
             c.database))
  with
  | Ok (name, false) -> `No_database name
  | Ok (_, true) | Error _ -> (e :> [> M.error ])

let with_connection ~sw ~net ~mono_clock c f =
  match connect ~sw ~net ~mono_clock c with
  | Ok conn -> closing conn f
  | Error e -> Error (missing ~sw ~net ~mono_clock c e)

(* On a switch of its own, so the connection is gone when [f] is, and a
   caller hands over no more than its environment. *)
let run env c f =
  Eio.Switch.run @@ fun sw ->
  with_connection ~sw ~net:env#net ~mono_clock:env#mono_clock c f

(* A database's name cannot be a parameter either; quoted, it is any name a
   URL can carry. *)
let quoted name =
  "\"" ^ String.concat "\"\"" (String.split_on_char '"' name) ^ "\""

(* Made and dropped from the server's own database, [postgres], since no
   statement makes or drops the database it is connected to. *)
let on_server ~sw ~net ~mono_clock (c : Pg.Conninfo.t) f =
  with_connection ~sw ~net ~mono_clock (Pg.Conninfo.with_database c "postgres")
    (fun admin -> f admin c.database)

let create ~sw ~net ~mono_clock c =
  on_server ~sw ~net ~mono_clock c (fun admin name ->
      Pg.exec_raw admin ("create database " ^ quoted name))

(* Not [with (force)]: Postgres refuses to drop a database somebody is
   connected to, the one guard there is against the wrong URL. *)
let drop ~sw ~net ~mono_clock c =
  on_server ~sw ~net ~mono_clock c (fun admin name ->
      Pg.exec_raw admin ("drop database if exists " ^ quoted name))

(* ------------------------------------------------------------------ *)
(* The schema, as one file *)

let normalise_dump dump =
  String.split_on_char '\n' dump
  |> List.filter (fun l ->
      not
        (String.starts_with ~prefix:"-- Dumped from" l
        || String.starts_with ~prefix:"-- Dumped by" l))
  |> String.concat "\n"

let replace ~sub ~by s =
  let n = String.length sub in
  let b = Buffer.create (String.length s) in
  let rec go i =
    if i > String.length s - n then
      Buffer.add_string b (String.sub s i (String.length s - i))
    else if String.equal (String.sub s i n) sub then (
      Buffer.add_string b by;
      go (i + n))
    else (
      Buffer.add_char b s.[i];
      go (i + 1))
  in
  go 0;
  Buffer.contents b

let words ~template ~url ~database =
  String.split_on_char ' ' template
  |> List.filter (fun w -> not (String.equal w ""))
  |> List.map (fun w ->
      replace ~sub:"{url}" ~by:url w |> replace ~sub:"{database}" ~by:database)

let pg_dump_argv ~template ~url ~database ~restrict_key =
  words ~template ~url ~database
  @ [
      "--schema-only";
      "--no-owner";
      "--no-privileges";
      (* A dump carries a random key unless given one, and a file that
         changes on every run cannot be reviewed as a diff. *)
      "--restrict-key=" ^ restrict_key;
    ]

(* Not through a shell: nothing in the template is interpreted twice. Run as
   an Eio process, so a cancelled dump kills it with its switch and nothing
   else on the loop waits for it. What it writes as errors is the failure's
   words, not lines on the caller's terminal, with every secret masked: the
   URL is one of its arguments, and a program may say its arguments back. A
   failure names the program and never the arguments themselves. *)
let capture ~process_mgr ~secrets argv =
  let masked s =
    List.fold_left
      (fun s secret ->
        if String.equal secret "" then s
        else replace ~sub:secret ~by:"<secret>" s)
      (String.trim s) secrets
  in
  let said err =
    match masked (Buffer.contents err) with "" -> "" | m -> ": " ^ m
  in
  match argv with
  | [] -> Error (`Dump "no pg_dump command")
  | prog :: _ -> (
      let out = Buffer.create 65536 and err = Buffer.create 1024 in
      match
        Eio.Switch.run (fun sw ->
            Eio.Process.await
              (Eio.Process.spawn ~sw process_mgr
                 ~stdout:(Eio.Flow.buffer_sink out)
                 ~stderr:(Eio.Flow.buffer_sink err) argv))
      with
      | `Exited 0 -> Ok (Buffer.contents out)
      | `Exited n ->
          Error (`Dump (Printf.sprintf "%s exited with %d%s" prog n (said err)))
      | `Signaled n ->
          Error
            (`Dump
               (Printf.sprintf "%s was stopped by signal %d%s" prog n (said err)))
      | exception Eio.Io (Eio.Process.E (Eio.Process.Executable_not_found _), _)
        ->
          Error (`Dump (prog ^ ": no such program"))
      | exception Eio.Io (Eio.Process.E (Eio.Process.Permission_denied _), _) ->
          Error (`Dump (prog ^ ": not allowed to run it"))
      | exception Eio.Io (Eio.Process.E _, _) ->
          Error (`Dump (prog ^ ": it could not be run")))

(* What the libraries made, read from the scratch database once they have
   run: the tables an application's baseline leaves out, since their own
   migrations make them. Anything else a library made -- a type, a function,
   a view -- a baseline could not tell from the application's, so squashing
   over it is refused. *)
let made_by_libraries conn =
  let user = "not in ('pg_catalog', 'information_schema', 'pg_toast')" in
  let* tables =
    Pg.run conn
      (S.list ~params:S.unit ~row:S.text
         ("select format('%I.%I', schemaname, tablename) from pg_tables where \
           schemaname " ^ user))
      ()
  in
  let* others =
    Pg.run conn
      (S.find ~params:S.unit ~row:S.int
         ("select (select count(*) from pg_proc p join pg_namespace n on n.oid \
           = p.pronamespace where n.nspname " ^ user
        ^ ")::int + (select count(*) from pg_type t join pg_namespace n on \
           n.oid = t.typnamespace where t.typtype in ('e', 'd', 'r', 'm') and \
           n.nspname " ^ user
        ^ ")::int + (select count(*) from pg_class c join pg_namespace n on \
           n.oid = c.relnamespace where c.relkind in ('v', 'm', 'f') and \
           n.nspname " ^ user ^ ")::int"))
      ()
  in
  if others > 0 then Error `Not_only_tables else Ok tables

(* A database of its own, made empty, migrated, dumped, and dropped however
   that went -- so a schema is what the migrations make from nothing, and
   never what some database has drifted to. *)
let migrated ~sw ~net ~mono_clock ~process_mgr ~table ~libraries ~apart
    ~migrations (c : Pg.Conninfo.t) dump =
  let scratch = Printf.sprintf "rowtype_migrate_%d" (Unix.getpid ()) in
  with_connection ~sw ~net ~mono_clock c (fun admin ->
      let* () = Pg.exec_raw admin ("create database " ^ quoted scratch) in
      (* A drop that fails leaves a database on the server, which is said,
         and the work's own answer stands: the schema it dumped is sound. *)
      Fun.protect
        ~finally:(fun () ->
          match
            Eio.Cancel.protect (fun () ->
                Pg.exec_raw admin
                  ("drop database if exists " ^ quoted scratch ^ " with (force)"))
          with
          | Ok () -> ()
          | Error e ->
              Log.err (fun m ->
                  m
                    "the scratch database %s was not dropped, and is to be \
                     dropped by hand: %s"
                    scratch (S.error_to_string e)))
        (fun () ->
          let target = Pg.Conninfo.with_database c scratch in
          let* theirs =
            with_connection ~sw ~net ~mono_clock target (fun conn ->
                let* theirs =
                  if apart then
                    let* () = apply ~table ~libraries conn [] in
                    match made_by_libraries conn with
                    | Ok tables -> Ok tables
                    | Error `Not_only_tables -> Error M.made_more_than_tables
                    | Error (#S.error as e) -> Error e
                  else Ok []
                in
                let* () = apply ~table ~libraries conn migrations in
                Ok theirs)
          in
          let url = Pg.Conninfo.to_url target in
          capture ~process_mgr
            ~secrets:(url :: Option.to_list target.password)
            (dump ~url ~database:scratch
            @ List.map (fun t -> "--exclude-table=" ^ t) theirs)))

(* What a baseline is dumped as: the schema and every row the migrations
   wrote -- a seeded table is part of what they made -- as statements
   ([--inserts], since a COPY's rows would come from psql's input), and
   never the record, which the baseline is recorded in. *)
let baseline_argv ~template ~restrict_key ~table ~url ~database =
  words ~template ~url ~database
  @ [
      "--no-owner";
      "--no-privileges";
      "--inserts";
      "--exclude-table=" ^ table;
      "--restrict-key=" ^ restrict_key;
    ]

(* pg_dump writes psql's own commands -- [\restrict] -- which a server cannot
   run, and names its versions, which change nothing. *)
let statements dump =
  String.split_on_char '\n' (normalise_dump dump)
  |> List.filter (fun l -> not (String.starts_with ~prefix:"\\" l))
  |> String.concat "\n"

let dump ~sw ~net ~mono_clock ~process_mgr ?(table = M.default_table)
    ?(libraries = []) ?(pg_dump = "pg_dump --dbname={url}")
    ?(restrict_key = "rowtype") ~migrations c =
  let* table = M.table_name table in
  Result.map normalise_dump
    (migrated ~sw ~net ~mono_clock ~process_mgr ~table ~libraries ~apart:false
       ~migrations c
       (pg_dump_argv ~template:pg_dump ~restrict_key))

(* The schema and every row the migrations wrote, the record and the
   libraries' tables left out. *)
let squash ~sw ~net ~mono_clock ~process_mgr ?(table = M.default_table)
    ?(libraries = []) ?(pg_dump = "pg_dump --dbname={url}")
    ?(restrict_key = "rowtype") ~through ~migrations c =
  let* table = M.table_name table in
  M.squash ~through ~migrations ~dumped:(fun ~apart migrations ->
      Result.map statements
        (migrated ~sw ~net ~mono_clock ~process_mgr ~table ~libraries ~apart
           ~migrations c
           (baseline_argv ~template:pg_dump ~restrict_key ~table)))

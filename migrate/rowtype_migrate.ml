module S = Rowtype
module Pg = Rowtype_postgres

type kind = Transaction | No_transaction | Baseline
type migration = { version : int; name : string; kind : kind; sql : string }

let is_baseline m =
  match m.kind with Baseline -> true | Transaction | No_transaction -> false

let ( let* ) = Result.bind
let db r = Result.map_error S.error_to_string r

(* ------------------------------------------------------------------ *)
(* The files *)

(* A directive is a file's first line, where whoever reads the file sees it
   before anything it changes. One that is not known is refused rather than
   read as a comment: a misspelt [no transaction] would run in one, and fail
   on the statement it was written for. *)
let directive = "-- rowtype-migrate:"

let kind_of base sql =
  let first =
    String.trim
      (match String.index_opt sql '\n' with
      | Some i -> String.sub sql 0 i
      | None -> sql)
  in
  if not (String.starts_with ~prefix:directive first) then Ok Transaction
  else
    let n = String.length directive in
    match String.trim (String.sub first n (String.length first - n)) with
    | "no transaction" -> Ok No_transaction
    | "baseline" -> Ok Baseline
    | word ->
        Error
          (Printf.sprintf
             "%s: %S is not a directive: `no transaction` and `baseline` are"
             base word)

(* A version is a UTC timestamp to the second, [YYYYMMDDHHMMSS], so two
   branches that each add a migration collide only if they did it in the same
   second -- a merge somebody has to look at, which is why a version used
   twice is refused rather than ordered. *)
let version_digits = 14

let parse path =
  let base = Filename.basename path in
  let malformed =
    Error (base ^ ": a migration is <14-digit UTC timestamp>_<name>.sql")
  in
  let digit c = c >= '0' && c <= '9' in
  match Filename.chop_suffix_opt ~suffix:".sql" base with
  | None -> malformed
  | Some stem -> (
      match String.index_opt stem '_' with
      | Some i when i = version_digits -> (
          let version = String.sub stem 0 i
          and name = String.sub stem (i + 1) (String.length stem - i - 1) in
          match int_of_string_opt version with
          | Some v
            when String.for_all digit version && not (String.equal name "") -> (
              match In_channel.with_open_bin path In_channel.input_all with
              | sql ->
                  let* kind = kind_of base sql in
                  Ok { version = v; name; kind; sql }
              | exception Sys_error m -> Error m)
          | Some _ | None -> malformed)
      | Some _ | None -> malformed)

(* A baseline stands for every migration before it, so it is the oldest, and
   there is one. *)
let listed ms =
  let ms = List.sort (fun a b -> Int.compare a.version b.version) ms in
  match (List.filter is_baseline ms, ms) with
  | [], _ -> Ok ()
  | [ b ], oldest :: _ when oldest.version = b.version -> Ok ()
  | [ b ], _ ->
      Error
        (Printf.sprintf
           "migration %d_%s is a baseline and not the oldest: a baseline \
            stands for every migration before it"
           b.version b.name)
  | _ :: b :: _, _ ->
      Error
        (Printf.sprintf
           "migration %d_%s is a second baseline: there is one, the oldest"
           b.version b.name)

let of_files paths =
  let* ms =
    List.fold_left
      (fun acc p ->
        let* acc = acc in
        let* m = parse p in
        Ok (m :: acc))
      (Ok []) paths
  in
  let ms = List.sort (fun a b -> Int.compare a.version b.version) ms in
  let rec twice = function
    | a :: (b :: _ as rest) ->
        if a.version = b.version then
          Error (Printf.sprintf "two migrations at version %d" a.version)
        else twice rest
    | [ _ ] | [] -> Ok ()
  in
  let* () = twice ms in
  let* () = listed ms in
  Ok ms

let of_directory dir =
  match Sys.readdir dir with
  | exception Sys_error m -> Error m
  | files ->
      Array.to_list files
      |> List.filter (fun f -> Filename.check_suffix f ".sql")
      |> List.map (Filename.concat dir)
      |> of_files

let stamp now =
  let t = Unix.gmtime now in
  Printf.sprintf "%04d%02d%02d%02d%02d%02d" (t.tm_year + 1900) (t.tm_mon + 1)
    t.tm_mday t.tm_hour t.tm_min t.tm_sec

let valid_name n =
  String.length n > 0
  && String.for_all
       (function 'a' .. 'z' | '0' .. '9' | '_' -> true | _ -> false)
       n

(* Made where it is not there, so a project's first migration is its first
   command. *)
let rec directory dir =
  if Sys.file_exists dir then
    if Sys.is_directory dir then Ok ()
    else Error (dir ^ " is there, and is not a directory")
  else
    let* () = directory (Filename.dirname dir) in
    match Sys.mkdir dir 0o755 with
    | () -> Ok ()
    | exception Sys_error m -> Error m

let new_migration ~dir ~now name =
  if not (valid_name name) then
    Error
      (Printf.sprintf
         "%S is not a migration name: lower-case letters, digits and \
          underscores"
         name)
  else
    let stamp = stamp now in
    let taken =
      Array.exists
        (fun f -> String.starts_with ~prefix:(stamp ^ "_") f)
        (try Sys.readdir dir with Sys_error _ -> [||])
    in
    if taken then
      Error
        (Printf.sprintf
           "a migration at %s already exists: wait a second and ask again" stamp)
    else
      let* () = directory dir in
      let path = Filename.concat dir (stamp ^ "_" ^ name ^ ".sql") in
      match
        Out_channel.with_open_bin path (fun oc ->
            Out_channel.output_string oc
              ("-- " ^ name
             ^ ": what this changes, and why.\n\
                -- Forward only: once it has run anywhere, it is never edited.\n\n"
              ))
      with
      | () -> Ok path
      | exception Sys_error m -> Error m

(* ------------------------------------------------------------------ *)
(* The database's record *)

(* What was applied is known by its text, so a file edited after it ran is
   noticed rather than silently disagreeing with every database it ran on.
   A digest is enough: this guards against a mistake, not an adversary. *)
let sum m = Digest.to_hex (Digest.string m.sql)
let default_table = "schema_migrations"

(* The table is named inside statements -- a name cannot be a parameter --
   so it is held to a grammar with nothing in it to quote. *)
let table_name t =
  let identifier s =
    String.length s > 0
    && (match s.[0] with 'a' .. 'z' | '_' -> true | _ -> false)
    && String.for_all
         (function 'a' .. 'z' | '0' .. '9' | '_' -> true | _ -> false)
         s
  in
  match String.split_on_char '.' t with
  | ([ _ ] | [ _; _ ]) as parts when List.for_all identifier parts -> Ok t
  | _ ->
      Error
        (Printf.sprintf
           "%S is not a table's name: lower-case letters, digits and \
            underscores, after a schema and a dot if it names one"
           t)

(* The table and its checksum column, which a database migrated before
   there was one gains here; its rows' sums are recorded on first sight. *)
let applied ~table conn =
  let* () =
    db
      (Pg.exec_raw conn
         (Printf.sprintf
            {|create table if not exists %s (
                version bigint primary key,
                name text not null,
                applied_at timestamptz not null default now());
              alter table %s
                add column if not exists checksum text|}
            table table))
  in
  db
    (Pg.run conn
       (S.list ~params:S.unit
          ~row:(S.t2 S.int (S.opt S.text))
          (Printf.sprintf "select version, checksum from %s order by version"
             table))
       ())

let record ~table conn m =
  db
    (Pg.run conn
       (S.exec ~params:(S.t3 S.int S.text S.text)
          (Printf.sprintf
             "insert into %s (version, name, checksum) values ($1, $2, $3)"
             table))
       (m.version, m.name, sum m))

let failed m e = Printf.sprintf "migration %d_%s failed: %s" m.version m.name e

(* One transaction per file, with the row that records it, so a failure
   leaves the database at exactly the version before it.

   What Postgres will not run inside a transaction -- an index built
   concurrently, a VACUUM -- runs on its own, and its row is written after
   it: a run cut off between the two runs it again, which is why such a file
   has to be one that can run twice. *)
let apply ~table conn m =
  (* The row first, since in one transaction the order is not seen, and a
     migration that moves the search path -- a baseline does -- would
     otherwise leave the record's table out of it. *)
  let in_transaction () =
    let* () = db (Pg.exec_raw conn "begin") in
    match
      let* () = record ~table conn m in
      let* () = db (Pg.exec_raw conn m.sql) in
      db (Pg.commit conn)
    with
    | Ok `Committed -> Ok ()
    | Ok `Rolled_back ->
        Error (failed m "its transaction was aborted, and rolled back")
    | Error e ->
        ignore (Pg.exec_raw conn "rollback" : (unit, S.error) result);
        Error (failed m e)
  in
  match m.kind with
  | Transaction -> in_transaction ()
  | Baseline ->
      (* A baseline is a pg_dump, which sets the session's parameters as it
         starts -- an empty search path among them -- and never puts them
         back, so every migration after it would run in the dump's session.
         Put back, with the run's own unbounded statements. *)
      let* () = in_transaction () in
      db (Pg.exec_raw conn "reset all; set statement_timeout = 0")
  | No_transaction ->
      Result.map_error (failed m)
        (let* () = db (Pg.exec_raw conn m.sql) in
         record ~table conn m)

(* What a list and a database's record come to: the migrations to apply,
   in order, and the four ways the two can disagree. *)
type reading = {
  to_apply : migration list;
  unknown : int list;
  edited : migration list;
  late : migration list;
  unsummed : migration list;
  behind : migration option;
}

module Versions = Map.Make (Int)

(* Each side by its versions, so matching the two is a lookup and not a walk
   of the other list for every entry. *)
let read migrations recorded =
  let files =
    List.fold_left
      (fun acc m -> Versions.add m.version m acc)
      Versions.empty migrations
  and sums =
    List.fold_left
      (fun acc (v, s) -> Versions.add v s acc)
      Versions.empty recorded
  in
  let find v = Versions.find_opt v files in
  let recorded_as v = Versions.find_opt v sums in
  (* A baseline stands for every migration up to its version, whose files
     are gone. Where the database recorded that version -- by the migration
     the baseline replaced, or by the baseline itself -- it is applied, and
     what is recorded up to it is history, compared with nothing; where the
     database recorded something, but not that, it is behind the squash. *)
  let history, behind =
    match (List.find_opt is_baseline migrations, recorded) with
    | None, _ | Some _, [] -> ((fun _ -> false), None)
    | Some b, _ :: _ -> (
        match recorded_as b.version with
        | Some _ -> ((fun v -> v <= b.version), None)
        | None -> ((fun _ -> false), Some b))
  in
  let highest = List.fold_left (fun h (v, _) -> max h v) 0 recorded in
  let to_apply =
    List.filter (fun m -> Option.is_none (recorded_as m.version)) migrations
    |> List.sort (fun a b -> Int.compare a.version b.version)
  in
  let applied_with f =
    List.filter_map
      (fun (v, s) ->
        match find v with
        | Some m when (not (history v)) && f m s -> Some m
        | _ -> None)
      recorded
  in
  {
    to_apply;
    unknown =
      List.filter_map
        (fun (v, _) ->
          if Option.is_none (find v) && not (history v) then Some v else None)
        recorded;
    edited =
      applied_with (fun m -> function
        | Some s -> not (String.equal s (sum m))
        | None -> false);
    late = List.filter (fun m -> m.version < highest) to_apply;
    unsummed = applied_with (fun _ s -> Option.is_none s);
    behind;
  }

let edited_refusal m =
  Printf.sprintf
    "migration %d_%s was edited after it ran: its text is not what the \
     database applied, and a schema that has run changes by the next \
     migration, never by rewriting one"
    m.version m.name

let behind_refusal b =
  Printf.sprintf
    "the database's record stops before migration %d, the baseline that \
     replaced every migration up to it: migrate it with a build from before \
     the squash, then with this one"
    b.version

let refusal r =
  match (r.behind, r.unknown, r.edited, r.late) with
  | Some b, _, _, _ -> Some (behind_refusal b)
  | None, v :: _, _, _ ->
      Some
        (Printf.sprintf
           "the database has migration %d, which these files do not: it was \
            migrated from a newer checkout, and applying these would run a \
            history that is not its own"
           v)
  | None, [], m :: _, _ -> Some (edited_refusal m)
  | None, [], [], m :: _ ->
      Some
        (Printf.sprintf
           "migration %d_%s is older than one this database has applied: it \
            was merged after a later one ran, and would run out of the order \
            it was written in; give it a version later than every applied one"
           m.version m.name)
  | None, [], [], [] -> None

let pending ~table conn migrations =
  let* () = listed migrations in
  let* recorded = applied ~table conn in
  let r = read migrations recorded in
  match refusal r with
  | Some e -> Error e
  | None ->
      let* () =
        List.fold_left
          (fun acc m ->
            let* () = acc in
            db
              (Pg.run conn
                 (S.exec ~params:(S.t2 S.text S.int)
                    (Printf.sprintf
                       "update %s set checksum = $1 where version = $2" table))
                 (sum m, m.version)))
          (Ok ()) r.unsummed
      in
      List.fold_left
        (fun acc m ->
          let* () = acc in
          apply ~table conn m)
        (Ok ()) r.to_apply

(* Any fixed number, the same in every process that migrates one database:
   it is what makes two of them take turns. *)
let default_lock = 7_265_756_171

(* A migration is bounded by nothing but itself, and so is the wait for
   another process's: a connection may carry a statement timeout and a bound
   on its reads meant for requests, either of which would kill a long
   migration half-way. [reset] gives back the statement bound the
   connection was opened with, and the read bound is put back as it was,
   whatever the run ends in. A migration's failure is the answer over the
   release's, which a lost connection has already made. *)
let run ?(lock = default_lock) ?(table = default_table) conn migrations =
  let* table = table_name table in
  let reads = Pg.timeout conn in
  Pg.set_timeout conn None;
  Fun.protect ~finally:(fun () -> Pg.set_timeout conn reads) @@ fun () ->
  let* () = db (Pg.exec_raw conn "set statement_timeout = 0") in
  let* () =
    db (Pg.run conn (S.exec ~params:S.int "select pg_advisory_lock($1)") lock)
  in
  let outcome = pending ~table conn migrations in
  let released =
    let* () =
      db
        (Pg.run conn
           (S.exec ~params:S.int "select pg_advisory_unlock($1)")
           lock)
    in
    db (Pg.exec_raw conn "reset statement_timeout")
  in
  let* () = outcome in
  released

type status = {
  applied : (int * string) list;
  pending : migration list;
  unknown : int list;
  edited : int list;
  late : int list;
  behind : int option;
}

(* A read, so it creates nothing: a database never migrated has no table,
   and that is every migration pending. *)
let reading ~table conn migrations =
  let* table = table_name table in
  let* () = listed migrations in
  (* Found as the statements that write it find it, through the search
     path, whether or not its name says a schema. *)
  let* exists =
    db
      (Pg.run conn
         (S.find_opt ~params:S.text ~row:S.bool
            "select to_regclass($1) is not null")
         table)
  in
  (* A table from before the checksum column has no sums to compare, and a
     read writes nothing, so it does not add one. *)
  let* summed =
    match exists with
    | Some true ->
        db
          (Pg.run conn
             (S.find_opt ~params:S.text ~row:S.bool
                "select exists (select 1 from pg_attribute where attrelid = \
                 to_regclass($1) and attname = 'checksum' and not \
                 attisdropped)")
             table)
    | Some false | None -> Ok (Some false)
  in
  let* applied =
    match (exists, summed) with
    | Some true, Some true ->
        db
          (Pg.run conn
             (S.list ~params:S.unit
                ~row:(S.t3 S.int S.text (S.opt S.text))
                (Printf.sprintf
                   "select version, name, checksum from %s order by version"
                   table))
             ())
    | Some true, (Some false | None) ->
        Result.map
          (List.map (fun (v, n) -> (v, n, None)))
          (db
             (Pg.run conn
                (S.list ~params:S.unit ~row:(S.t2 S.int S.text)
                   (Printf.sprintf
                      "select version, name from %s order by version" table))
                ()))
    | (Some false | None), _ -> Ok []
  in
  Ok (applied, read migrations (List.map (fun (v, _, s) -> (v, s)) applied))

let status ?(table = default_table) conn migrations =
  let* applied, r = reading ~table conn migrations in
  let versions = List.map (fun m -> m.version) in
  Ok
    {
      applied = List.map (fun (v, n, _) -> (v, n)) applied;
      pending = r.to_apply;
      unknown = r.unknown;
      edited = versions r.edited;
      late = versions r.late;
      behind = Option.map (fun m -> m.version) r.behind;
    }

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

(* Not through a shell: nothing in the template is interpreted twice. A
   failure names the program and never its arguments, which hold the URL and
   so its password. *)
let capture argv =
  match argv with
  | [] -> Error "no pg_dump command"
  | prog :: _ -> (
      match Unix.open_process_args_in prog (Array.of_list argv) with
      | exception Unix.Unix_error (e, _, _) ->
          Error (Printf.sprintf "%s: %s" prog (Unix.error_message e))
      | ic -> (
          let out = In_channel.input_all ic in
          match Unix.close_process_in ic with
          | Unix.WEXITED 0 -> Ok out
          | Unix.WEXITED n -> Error (Printf.sprintf "%s exited with %d" prog n)
          | Unix.WSIGNALED n | Unix.WSTOPPED n ->
              Error (Printf.sprintf "%s was stopped by signal %d" prog n)))

(* A connect is bounded, since a server that never answers is otherwise a
   command that never ends; what runs on the connection is not, since an
   administrator's statement -- a database made or dropped, a migration --
   runs as long as it takes. *)
let connect_timeout_s = 10.

(* Quiet: [create table if not exists] is a NOTICE on every run, and a notice
   is a line in what a command prints. *)
let connect ~sw ~net ~mono_clock url =
  let* c = Result.map_error S.error_to_string (Pg.conninfo url) in
  let c =
    {
      c with
      connect_timeout_s =
        Some (Option.value c.connect_timeout_s ~default:connect_timeout_s);
    }
  in
  Result.map_error S.error_to_string
    (Pg.connect ~sw ~net ~mono_clock
       ~parameters:[ ("client_min_messages", "warning") ]
       c)

(* A finaliser runs in a fiber that may be cancelled, where any IO raises
   again: shielded, it does its work, and the cancellation goes on as itself
   rather than wrapped in [Fun.Finally_raised]. *)
let closing conn f =
  Fun.protect
    ~finally:(fun () -> Eio.Cancel.protect (fun () -> Pg.close conn))
    (fun () -> f conn)

(* A database that is not there is the one failure to connect with a next
   step to name, and the server's own database says whether it is that. *)
let missing ~sw ~net ~mono_clock url e =
  match
    let* c = Result.map_error S.error_to_string (Pg.conninfo url) in
    let* server = db (Pg.on_database ~server:url "postgres") in
    let* admin = connect ~sw ~net ~mono_clock server in
    closing admin (fun admin ->
        Result.map
          (fun there -> (c.database, there))
          (db
             (Pg.run admin
                (S.find ~params:S.text ~row:S.bool
                   "select exists (select 1 from pg_database where datname = \
                    $1)")
                c.database)))
  with
  | Ok (name, false) ->
      Printf.sprintf
        "there is no database %S on that server: make it with `rowtype-migrate \
         create`"
        name
  | Ok (_, true) | Error _ -> e

let with_connection ~sw ~net ~mono_clock url f =
  match connect ~sw ~net ~mono_clock url with
  | Ok conn -> closing conn f
  | Error e -> Error (missing ~sw ~net ~mono_clock url e)

(* A database's name cannot be a parameter either; quoted, it is any name a
   URL can carry. *)
let quoted name =
  "\"" ^ String.concat "\"\"" (String.split_on_char '"' name) ^ "\""

(* Made and dropped from the server's own database, [postgres], since no
   statement makes or drops the database it is connected to. *)
let on_server ~sw ~net ~mono_clock url f =
  let* c = Result.map_error S.error_to_string (Pg.conninfo url) in
  let* server = db (Pg.on_database ~server:url "postgres") in
  with_connection ~sw ~net ~mono_clock server (fun admin -> f admin c.database)

let create ~sw ~net ~mono_clock url =
  on_server ~sw ~net ~mono_clock url (fun admin name ->
      db (Pg.exec_raw admin ("create database " ^ quoted name)))

(* Not [with (force)]: Postgres refuses to drop a database somebody is
   connected to, the one guard there is against the wrong URL. *)
let drop ~sw ~net ~mono_clock url =
  on_server ~sw ~net ~mono_clock url (fun admin name ->
      db (Pg.exec_raw admin ("drop database if exists " ^ quoted name)))

(* A database of its own, made empty, migrated, dumped, and dropped however
   that went -- so a schema is what the migrations make from nothing, and
   never what some database has drifted to. *)
let migrated_scratch ~sw ~net ~mono_clock ?lock ~table ~migrations url dump =
  let scratch = Printf.sprintf "rowtype_migrate_%d" (Unix.getpid ()) in
  with_connection ~sw ~net ~mono_clock url (fun admin ->
      let* () = db (Pg.exec_raw admin ("create database " ^ quoted scratch)) in
      Fun.protect
        ~finally:(fun () ->
          Eio.Cancel.protect (fun () ->
              ignore
                (Pg.exec_raw admin
                   ("drop database if exists " ^ quoted scratch
                  ^ " with (force)")
                  : (unit, S.error) result)))
        (fun () ->
          let* target = db (Pg.on_database ~server:url scratch) in
          let* () =
            with_connection ~sw ~net ~mono_clock target (fun conn ->
                run ?lock ~table conn migrations)
          in
          capture (dump ~url:target ~database:scratch)))

let dump ~sw ~net ~mono_clock ?lock ?(table = default_table) ~pg_dump
    ~restrict_key ~migrations url =
  Result.map normalise_dump
    (migrated_scratch ~sw ~net ~mono_clock ?lock ~table ~migrations url
       (pg_dump_argv ~template:pg_dump ~restrict_key))

(* ------------------------------------------------------------------ *)
(* A history, as one file *)

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

let squash ~sw ~net ~mono_clock ?lock ?(table = default_table) ~pg_dump
    ~restrict_key ~through ~migrations url =
  let* table = table_name table in
  let* () = listed migrations in
  let replaced, kept =
    List.partition (fun m -> m.version <= through) migrations
  in
  let* () =
    match (List.exists (fun m -> m.version = through) migrations, replaced) with
    | false, _ ->
        Error
          (Printf.sprintf
             "there is no migration %d: a squash is through one of the files"
             through)
    | true, [ m ] when is_baseline m ->
        Error
          (Printf.sprintf
             "migration %d is a baseline already: there is nothing before it \
              to squash"
             through)
    | true, _ -> Ok ()
  in
  let dumped ms =
    Result.map statements
      (migrated_scratch ~sw ~net ~mono_clock ?lock ~table ~migrations:ms url
         (baseline_argv ~template:pg_dump ~restrict_key ~table))
  in
  let* made = dumped replaced in
  let baseline =
    {
      version = through;
      name = "baseline";
      kind = Baseline;
      sql =
        Printf.sprintf
          "-- rowtype-migrate: baseline\n\
           -- Every migration through %d, as the database they made: written by\n\
           -- `rowtype-migrate squash`, and never edited.\n\
           %s"
          through made;
    }
  in
  (* Proved before a file is touched: the baseline and what follows it make,
     row for row, the database the whole history makes. *)
  let* whole = dumped migrations in
  let* squashed = dumped (baseline :: kept) in
  if String.equal whole squashed then Ok baseline
  else
    Error
      "the baseline and the migrations after it do not make the database the \
       whole history makes, so nothing was squashed"

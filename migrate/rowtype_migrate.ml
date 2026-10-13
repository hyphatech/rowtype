module S = Rowtype
module Pg = Rowtype_postgres

type kind = Transaction | No_transaction | Baseline

(* A version is a UTC timestamp to the second, [YYYYMMDDHHMMSS], so two
   branches that each add a migration collide only if they did it in the same
   second -- a merge somebody has to look at, which is why a version used
   twice is refused rather than ordered. The interface keeps it private, so a
   version a program makes has fourteen digits; one a database recorded is
   whatever it recorded. *)
type version = int

let version_digits = 14

(* The numbers of fourteen digits. *)
let lowest_version = 10_000_000_000_000
let highest_version = 99_999_999_999_999

let version_of_int n =
  if n >= lowest_version && n <= highest_version then Some n else None

type migration = { version : version; name : string; kind : kind; sql : string }

type error =
  [ S.error
  | `Files of string
  | `Invalid_argument of string
  | `Record of
    [ `Behind of migration
    | `Unknown of version
    | `Edited of migration
    | `Late of migration ]
  | `Migration_failed of migration * S.error
  | `No_database of string
  | `Dump of string
  | `Unproved ]

let error_to_string : [< error ] -> string = function
  | `Files m | `Invalid_argument m | `Dump m -> m
  | `Record (`Behind b) ->
      Printf.sprintf
        "the database's record stops before migration %d, the baseline that \
         replaced every migration up to it: migrate it with a build from \
         before the squash, then with this one"
        b.version
  | `Record (`Unknown v) ->
      Printf.sprintf
        "the database has migration %d, which these files do not: it was \
         migrated from a newer checkout, and applying these would run a \
         history that is not its own"
        v
  | `Record (`Edited m) ->
      Printf.sprintf
        "migration %d_%s was edited after it ran: its text is not what the \
         database applied, and a schema that has run changes by the next \
         migration, never by rewriting one"
        m.version m.name
  | `Record (`Late m) ->
      Printf.sprintf
        "migration %d_%s is older than one this database has applied: it was \
         merged after a later one ran, and would run out of the order it was \
         written in; give it a version later than every applied one"
        m.version m.name
  | `Migration_failed (m, e) ->
      Printf.sprintf "migration %d_%s failed: %s" m.version m.name
        (S.error_to_string e)
  | `No_database name ->
      Printf.sprintf
        "there is no database %S on that server: make it with `rowtype-migrate \
         create`"
        name
  | `Unproved ->
      "the baseline and the migrations after it do not make the database the \
       whole history makes, so nothing was squashed"
  | #S.error as e -> S.error_to_string e

let is_baseline m =
  match m.kind with Baseline -> true | Transaction | No_transaction -> false

let ( let* ) = Result.bind

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
          (`Files
             (Printf.sprintf
                "%s: %S is not a directive: `no transaction` and `baseline` are"
                base word))

let parse path =
  let base = Filename.basename path in
  let malformed =
    Error
      (`Files
         (Printf.sprintf
            "%s: a migration is <%d-digit UTC timestamp>_<name>.sql" base
            version_digits))
  in
  let digit = function '0' .. '9' -> true | _ -> false in
  match Filename.chop_suffix_opt ~suffix:".sql" base with
  | None -> malformed
  | Some stem -> (
      match String.index_opt stem '_' with
      | Some i when i = version_digits -> (
          let version = String.sub stem 0 i
          and name = String.sub stem (i + 1) (String.length stem - i - 1) in
          match Option.bind (int_of_string_opt version) version_of_int with
          | Some v
            when String.for_all digit version && not (String.equal name "") -> (
              match In_channel.with_open_bin path In_channel.input_all with
              | sql ->
                  let* kind = kind_of base sql in
                  Ok { version = v; name; kind; sql }
              | exception Sys_error m -> Error (`Files m))
          | Some _ | None -> malformed)
      | Some _ | None -> malformed)

(* A baseline stands for every migration before it, so it is the oldest, and
   there is one. *)
let listed migrations =
  let migrations =
    List.sort (fun a b -> Int.compare a.version b.version) migrations
  in
  match (List.filter is_baseline migrations, migrations) with
  | [], _ -> Ok ()
  | [ b ], oldest :: _ when oldest.version = b.version -> Ok ()
  | [ b ], _ ->
      Error
        (`Files
           (Printf.sprintf
              "migration %d_%s is a baseline and not the oldest: a baseline \
               stands for every migration before it"
              b.version b.name))
  | _ :: b :: _, _ ->
      Error
        (`Files
           (Printf.sprintf
              "migration %d_%s is a second baseline: there is one, the oldest"
              b.version b.name))

let of_files paths =
  let* migrations =
    List.fold_left
      (fun acc p ->
        let* acc = acc in
        let* m = parse p in
        Ok (m :: acc))
      (Ok []) paths
  in
  let migrations =
    List.sort (fun a b -> Int.compare a.version b.version) migrations
  in
  let rec twice = function
    | a :: (b :: _ as rest) ->
        if a.version = b.version then
          Error
            (`Files (Printf.sprintf "two migrations at version %d" a.version))
        else twice rest
    | [ _ ] | [] -> Ok ()
  in
  let* () = twice migrations in
  let* () = listed migrations in
  Ok migrations

let of_directory dir =
  match Sys.readdir dir with
  | exception Sys_error m -> Error (`Files m)
  | files ->
      Array.to_list files
      |> List.filter (fun f -> Filename.check_suffix f ".sql")
      |> List.map (Filename.concat dir)
      |> of_files

(* ------------------------------------------------------------------ *)
(* Sources *)

(* The application's history is [app] in every database, and set by no
   flag: a name one machine passed and another did not would be a history
   applied again from its start. *)
let app = "app"

type source = { name : string; migrations : migration list }

let source_name n =
  String.length n > 0
  && (match n.[0] with 'a' .. 'z' -> true | _ -> false)
  && String.for_all
       (function 'a' .. 'z' | '0' .. '9' | '_' | '-' -> true | _ -> false)
       n

let source name migrations =
  if String.equal name app then
    Error
      (`Invalid_argument
         (Printf.sprintf
            "%S is the application's own history, and no library's name" name))
  else if not (source_name name) then
    Error
      (`Invalid_argument
         (Printf.sprintf
            "%S is not a source's name: lower-case letters, digits, hyphens \
             and underscores, starting with a letter, as a package's is"
            name))
  else Ok { name; migrations }

let of_package ~prefix package =
  if not (source_name package) then
    Error
      (`Invalid_argument (Printf.sprintf "%S is not a package's name" package))
  else
    of_directory
      (List.fold_left Filename.concat prefix [ "share"; package; "migrations" ])

let stamp now =
  let (year, month, day), ((hour, minute, second), _) =
    Ptime.to_date_time now
  in
  Printf.sprintf "%04d%02d%02d%02d%02d%02d" year month day hour minute second

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
    else Error (`Files (dir ^ " is there, and is not a directory"))
  else
    let* () = directory (Filename.dirname dir) in
    match Sys.mkdir dir 0o755 with
    | () -> Ok ()
    | exception Sys_error m -> Error (`Files m)

let new_migration ~dir ~now name =
  if not (valid_name name) then
    Error
      (`Invalid_argument
         (Printf.sprintf
            "%S is not a migration name: lower-case letters, digits and \
             underscores"
            name))
  else
    let stamp = stamp now in
    let taken =
      Array.exists
        (fun f -> String.starts_with ~prefix:(stamp ^ "_") f)
        (try Sys.readdir dir with Sys_error _ -> [||])
    in
    if taken then
      Error
        (`Files
           (Printf.sprintf
              "a migration at %s already exists: wait a second and ask again"
              stamp))
    else
      let* () = directory dir in
      let path = Filename.concat dir (stamp ^ "_" ^ name ^ ".sql") in
      match
        Out_channel.with_open_bin path (fun oc ->
            Out_channel.output_string oc
              ("-- " ^ name ^ ": what this changes, and why.\n\n"))
      with
      | () -> Ok path
      | exception Sys_error m -> Error (`Files m)

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
        (`Invalid_argument
           (Printf.sprintf
              "%S is not a table's name: lower-case letters, digits and \
               underscores, starting with a letter or an underscore, after a \
               schema and a dot if it names one"
              t))

(* The table, made with every column it has today, and a table made before
   brought up to it: the checksum, whose rows' sums are recorded on first
   sight, and the source, whose rows are the application's -- the only
   history there was -- and which joins the version in the key, since a
   version is unique within its source. Made under the run's lock, so two
   processes never both bring one table up. *)
let prepare ~table conn =
  let* () =
    Pg.exec_raw conn
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
         table app table table app)
  in
  let* key =
    Pg.run conn
      (S.find_opt ~params:S.text ~row:(S.t2 S.text S.int)
         "select conname::text, cardinality(conkey) from pg_constraint where \
          conrelid = to_regclass($1) and contype = 'p'")
      table
  in
  match key with
  | Some (name, 1) ->
      Pg.exec_raw conn
        (Printf.sprintf
           "alter table %s drop constraint \"%s\", add primary key (source, \
            version)"
           table name)
  | Some _ | None -> Ok ()

let applied ~table ~source conn =
  Pg.run conn
    (S.list ~params:S.text
       ~row:(S.t2 S.int (S.opt S.text))
       (Printf.sprintf
          "select version, checksum from %s where source = $1 order by version"
          table))
    source

let record ~table ~source conn m =
  Pg.run conn
    (S.exec
       ~params:(S.t4 S.text S.int S.text S.text)
       (Printf.sprintf
          "insert into %s (source, version, name, checksum) values ($1, $2, \
           $3, $4)"
          table))
    (source, m.version, m.name, sum m)

let failed m = function
  | Ok v -> Ok v
  | Error e -> Error (`Migration_failed (m, e))

(* One transaction per file, with the row that records it, so a failure
   leaves the database at exactly the version before it. Run once, with no
   retry, since a file run twice is a schema changed twice.

   A file that ends that transaction itself -- a [commit], a [rollback] --
   runs the rest of it outside one, and a [commit] there has kept the
   record: it is refused, and its record removed. The transaction is known by its
   id, so a file that begins another after its end is refused too.

   What Postgres will not run inside a transaction -- an index built
   concurrently, a VACUUM -- runs on its own, and its row is written after
   it: a run cut off between the two runs it again, which is why such a file
   has to be one that can run twice. *)
let apply ~table ~source conn m =
  let ended =
    "it ends the transaction it runs in, so it was not applied as one: its \
     record is removed, and what ran before that end stays as the end left it. \
     A file that runs outside a transaction says so on its first line, `-- \
     rowtype-migrate: no transaction`"
  in
  let recorded () =
    Pg.run conn
      (S.find ~params:(S.t2 S.text S.int) ~row:S.bool
         (Printf.sprintf
            "select exists (select from %s where source = $1 and version = $2)"
            table))
      (source, m.version)
  in
  let unrecord () =
    Pg.run conn
      (S.exec ~params:(S.t2 S.text S.int)
         (Printf.sprintf "delete from %s where source = $1 and version = $2"
            table))
      (source, m.version)
  in
  (* The row first, since in one transaction the order is not seen, and a
     migration that moves the search path -- a baseline does -- would
     otherwise leave the record's table out of it. *)
  let in_transaction () =
    match
      Pg.Transaction.within conn (fun conn ->
          let* () = record ~table ~source conn m in
          let* id =
            Pg.run conn
              (S.find ~params:S.unit ~row:S.int "select txid_current()")
              ()
          in
          let* () = Pg.exec_raw conn m.sql in
          let* still =
            Pg.run conn
              (S.find ~params:S.unit ~row:(S.opt S.int)
                 "select txid_current_if_assigned()")
              ()
          in
          if Option.equal Int.equal (Some id) still then Ok () else Error `Ended)
    with
    | Ok () -> Ok ()
    | Error `Ended ->
        let* () = failed m (unrecord ()) in
        Error (`Migration_failed (m, `Db ended))
    | Error (`Not_committed detail) -> Error (`Migration_failed (m, `Db detail))
    | Error (#S.error as e) -> (
        (* The record outlives the rollback only where the file committed
           it before it failed. *)
        match recorded () with
        | Ok true ->
            let* () = failed m (unrecord ()) in
            Error
              (`Migration_failed
                 (m, `Db (ended ^ ". It then failed: " ^ S.error_to_string e)))
        | Ok false | Error _ -> Error (`Migration_failed (m, e)))
  in
  let applied =
    match m.kind with
    | Transaction | Baseline -> in_transaction ()
    | No_transaction ->
        failed m
          (let* () = Pg.exec_raw conn m.sql in
           record ~table ~source conn m)
  in
  (* A [set] outlives the transaction it ran in -- a pg_dump, which a
     baseline is, sets an empty search path -- and would carry into every
     later migration on the connection, so a schema would depend on how its
     migrations were grouped into runs. Put back after each, with the run's
     own unbounded statements; the migration's failure is the answer over
     the reset's. *)
  let reset = Pg.exec_raw conn "reset all; set statement_timeout = 0" in
  let* () = applied in
  reset

(* What a list and a database's record come to: the migrations to apply,
   in order, and the four ways the two can disagree. *)
type reading = {
  to_apply : migration list;
  unknown : version list;
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

(* The first way a list and a record disagree, in the order a person would
   untangle them: a database behind a squash, then one from a newer
   checkout, then an edited file, then a late one. *)
let refusal r =
  match (r.behind, r.unknown, r.edited, r.late) with
  | Some b, _, _, _ -> Some (`Behind b)
  | None, v :: _, _, _ -> Some (`Unknown v)
  | None, [], m :: _, _ -> Some (`Edited m)
  | None, [], [], m :: _ -> Some (`Late m)
  | None, [], [], [] -> None

(* One source's migrations against its own rows of the record: every check
   is made within the source, so a library's migration is never late beside
   an application's. *)
let pending ~table ~source conn migrations =
  let* () = listed migrations in
  let* recorded = applied ~table ~source conn in
  let r = read migrations recorded in
  match refusal r with
  | Some refused -> Error (`Record refused)
  | None ->
      let* () =
        List.fold_left
          (fun acc m ->
            let* () = acc in
            Pg.run conn
              (S.exec ~params:(S.t3 S.text S.text S.int)
                 (Printf.sprintf
                    "update %s set checksum = $1 where source = $2 and version \
                     = $3"
                    table))
              (sum m, source, m.version))
          (Ok ()) r.unsummed
      in
      List.fold_left
        (fun acc m ->
          let* () = acc in
          apply ~table ~source conn m)
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
(* Two libraries of one name would be one history read twice. *)
let distinct libraries =
  let rec go seen = function
    | [] -> Ok ()
    | l :: rest ->
        if List.exists (String.equal l.name) seen then
          Error
            (`Invalid_argument
               (Printf.sprintf "the source %S is given twice" l.name))
        else go (l.name :: seen) rest
  in
  go [] libraries

(* The libraries in the order given, then the application, which may stand
   on their tables. *)
let sources libraries migrations =
  List.map (fun l -> (l.name, l.migrations)) libraries @ [ (app, migrations) ]

let run ?(lock = default_lock) ?(table = default_table) ?(libraries = []) conn
    migrations =
  let* table = table_name table in
  let* () = distinct libraries in
  let reads = Pg.timeout conn in
  Pg.set_timeout conn ~timeout:None;
  Fun.protect ~finally:(fun () -> Pg.set_timeout conn ~timeout:reads)
  @@ fun () ->
  let* () = Pg.exec_raw conn "set statement_timeout = 0" in
  let* () =
    Pg.run conn (S.exec ~params:S.int "select pg_advisory_lock($1)") lock
  in
  let outcome =
    let* () = prepare ~table conn in
    List.fold_left
      (fun acc (source, migrations) ->
        let* () = acc in
        pending ~table ~source conn migrations)
      (Ok ())
      (sources libraries migrations)
  in
  let released =
    let* () =
      Pg.run conn (S.exec ~params:S.int "select pg_advisory_unlock($1)") lock
    in
    Pg.exec_raw conn "reset statement_timeout"
  in
  let* () = outcome in
  released

type status = {
  source : string;
  applied : (version * string) list;
  pending : migration list;
  unknown : version list;
  edited : version list;
  late : version list;
  behind : version option;
}

(* A read, so it creates nothing: a database never migrated has no table,
   and that is every migration pending; one from before the source column
   holds the application's history alone. *)
let reading ~table ~source conn migrations =
  let* () = listed migrations in
  (* Found as the statements that write it find it, through the search
     path, whether or not its name says a schema. *)
  let* exists =
    Pg.run conn
      (S.find_opt ~params:S.text ~row:S.bool
         "select to_regclass($1) is not null")
      table
  in
  (* A table from before the checksum column has no sums to compare, and a
     read writes nothing, so it does not add one. *)
  let* summed =
    match exists with
    | Some true ->
        Pg.run conn
          (S.find_opt ~params:S.text ~row:S.bool
             "select exists (select 1 from pg_attribute where attrelid = \
              to_regclass($1) and attname = 'checksum' and not attisdropped)")
          table
    | Some false | None -> Ok (Some false)
  in
  let* sourced =
    match exists with
    | Some true ->
        Pg.run conn
          (S.find_opt ~params:S.text ~row:S.bool
             "select exists (select 1 from pg_attribute where attrelid = \
              to_regclass($1) and attname = 'source' and not attisdropped)")
          table
    | Some false | None -> Ok (Some false)
  in
  let* applied =
    match (exists, summed, sourced) with
    | Some true, _, Some true ->
        Pg.run conn
          (S.list ~params:S.text
             ~row:(S.t3 S.int S.text (S.opt S.text))
             (Printf.sprintf
                "select version, name, checksum from %s where source = $1 \
                 order by version"
                table))
          source
    | Some true, _, (Some false | None) when not (String.equal source app) ->
        Ok []
    | Some true, Some true, (Some false | None) ->
        Pg.run conn
          (S.list ~params:S.unit
             ~row:(S.t3 S.int S.text (S.opt S.text))
             (Printf.sprintf
                "select version, name, checksum from %s order by version" table))
          ()
    | Some true, (Some false | None), (Some false | None) ->
        Result.map
          (List.map (fun (v, n) -> (v, n, None)))
          (Pg.run conn
             (S.list ~params:S.unit ~row:(S.t2 S.int S.text)
                (Printf.sprintf "select version, name from %s order by version"
                   table))
             ())
    | (Some false | None), _, _ -> Ok []
  in
  Ok (applied, read migrations (List.map (fun (v, _, s) -> (v, s)) applied))

let status ?(table = default_table) ?(libraries = []) conn migrations =
  let* table = table_name table in
  let* () = distinct libraries in
  let versions = List.map (fun m -> m.version) in
  List.fold_left
    (fun acc (source, migrations) ->
      let* acc = acc in
      let* applied, r = reading ~table ~source conn migrations in
      Ok
        ({
           source;
           applied = List.map (fun (v, n, _) -> (v, n)) applied;
           pending = r.to_apply;
           unknown = r.unknown;
           edited = versions r.edited;
           late = versions r.late;
           behind = Option.map (fun m -> m.version) r.behind;
         }
        :: acc))
    (Ok [])
    (sources libraries migrations)
  |> Result.map List.rev

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

(* A connect is bounded, since a server that never answers is otherwise a
   command that never ends; what runs on the connection is not, since an
   administrator's statement -- a database made or dropped, a migration --
   runs as long as it takes. *)
let connect_timeout = Mtime.Span.(10 * s)

(* Quiet: [create table if not exists] is a NOTICE on every run, and a notice
   is a line in what a command prints. *)
let connect ~sw ~net ~mono_clock url =
  let* c = Pg.conninfo url in
  let c =
    Pg.Conninfo.with_connect_timeout c
      (Option.value c.connect_timeout ~default:connect_timeout)
  in
  Pg.connect ~sw ~net ~mono_clock
    ~parameters:[ ("client_min_messages", "warning") ]
    c

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
    let* c = Pg.conninfo url in
    let* server = Pg.on_database ~server:url "postgres" in
    let* admin = connect ~sw ~net ~mono_clock server in
    closing admin (fun admin ->
        Result.map
          (fun there -> (c.database, there))
          (Pg.run admin
             (S.find ~params:S.text ~row:S.bool
                "select exists (select 1 from pg_database where datname = $1)")
             c.database))
  with
  | Ok (name, false) -> `No_database name
  | Ok (_, true) | Error _ -> (e :> [> error ])

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
  let* c = Pg.conninfo url in
  let* server = Pg.on_database ~server:url "postgres" in
  with_connection ~sw ~net ~mono_clock server (fun admin -> f admin c.database)

let create ~sw ~net ~mono_clock url =
  on_server ~sw ~net ~mono_clock url (fun admin name ->
      Pg.exec_raw admin ("create database " ^ quoted name))

(* Not [with (force)]: Postgres refuses to drop a database somebody is
   connected to, the one guard there is against the wrong URL. *)
let drop ~sw ~net ~mono_clock url =
  on_server ~sw ~net ~mono_clock url (fun admin name ->
      Pg.exec_raw admin ("drop database if exists " ^ quoted name))

(* A database of its own, made empty, migrated, dumped, and dropped however
   that went -- so a schema is what the migrations make from nothing, and
   never what some database has drifted to. *)
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
  if others > 0 then
    Error
      (`Invalid_argument
         "a library's migrations made something other than tables, which a \
          baseline cannot tell from the application's own: squash its history \
          without those libraries")
  else Ok tables

let migrated_scratch ~sw ~net ~mono_clock ~process_mgr ?lock ~table
    ?(libraries = []) ?(apart = false) ~migrations url dump =
  let scratch = Printf.sprintf "rowtype_migrate_%d" (Unix.getpid ()) in
  with_connection ~sw ~net ~mono_clock url (fun admin ->
      let* () = Pg.exec_raw admin ("create database " ^ quoted scratch) in
      Fun.protect
        ~finally:(fun () ->
          Eio.Cancel.protect (fun () ->
              ignore
                (Pg.exec_raw admin
                   ("drop database if exists " ^ quoted scratch
                  ^ " with (force)")
                  : (unit, S.error) result)))
        (fun () ->
          let* target = Pg.on_database ~server:url scratch in
          let* theirs =
            with_connection ~sw ~net ~mono_clock target (fun conn ->
                let* theirs =
                  if apart then
                    let* () = run ?lock ~table ~libraries conn [] in
                    made_by_libraries conn
                  else Ok []
                in
                let* () = run ?lock ~table ~libraries conn migrations in
                Ok theirs)
          in
          let* c = Pg.conninfo target in
          capture ~process_mgr
            ~secrets:(target :: Option.to_list c.password)
            (dump ~url:target ~database:scratch
            @ List.map (fun t -> "--exclude-table=" ^ t) theirs)))

let dump ~sw ~net ~mono_clock ~process_mgr ?lock ?(table = default_table)
    ?libraries ~pg_dump ~restrict_key ~migrations url =
  Result.map normalise_dump
    (migrated_scratch ~sw ~net ~mono_clock ~process_mgr ?lock ~table ?libraries
       ~migrations url
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

let squash ~sw ~net ~mono_clock ~process_mgr ?lock ?(table = default_table)
    ?libraries ~pg_dump ~restrict_key ~through ~migrations url =
  let* table = table_name table in
  let* () = listed migrations in
  let replaced, kept =
    List.partition (fun m -> m.version <= through) migrations
  in
  let* () =
    match (List.exists (fun m -> m.version = through) migrations, replaced) with
    | false, _ ->
        Error
          (`Invalid_argument
             (Printf.sprintf
                "there is no migration %d: a squash is through one of the files"
                through))
    | true, [ m ] when is_baseline m ->
        Error
          (`Invalid_argument
             (Printf.sprintf
                "migration %d is a baseline already: there is nothing before \
                 it to squash"
                through))
    | true, _ -> Ok ()
  in
  let dumped ~apart migrations =
    Result.map statements
      (migrated_scratch ~sw ~net ~mono_clock ~process_mgr ?lock ~table
         ?libraries ~apart ~migrations url
         (baseline_argv ~template:pg_dump ~restrict_key ~table))
  in
  let* made = dumped ~apart:true replaced in
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
  let* whole = dumped ~apart:false migrations in
  let* squashed = dumped ~apart:false (baseline :: kept) in
  if String.equal whole squashed then Ok baseline else Error `Unproved

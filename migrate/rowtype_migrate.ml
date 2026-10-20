module S = Rowtype

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
  | `Unproved
  | `Lock of [ `Lost ] ]

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
        "there is no database %S: make it with `rowtype-migrate create`" name
  | `Unproved ->
      "the baseline and the migrations after it do not make the database the \
       whole history makes, so nothing was squashed"
  | `Lock `Lost ->
      "the connection holding the migrations' lock was lost, and the lock with \
       it: nothing more was applied, and running again applies what is left"
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

let failed m = function
  | Ok v -> Ok v
  | Error e -> Error (`Migration_failed (m, e))

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
  and rows =
    List.fold_left
      (fun acc (v, name, s) -> Versions.add v (name, s) acc)
      Versions.empty recorded
  in
  let find v = Versions.find_opt v files in
  let recorded_as v = Versions.find_opt v rows in
  (* A baseline stands for every migration up to its version, whose files
     are gone. Where the database recorded that version it is applied, and
     what is recorded below it is history, compared with nothing; where the
     database recorded something, but not that, it is behind the squash.
     The version itself is history where the database recorded the
     migration the baseline replaced, known by its name, and held to its
     text where it recorded the baseline, which is what ran there. *)
  let history, behind =
    match (List.find_opt is_baseline migrations, recorded) with
    | None, _ | Some _, [] -> ((fun _ -> false), None)
    | Some b, _ :: _ -> (
        match recorded_as b.version with
        | Some (name, _) when String.equal name b.name ->
            ((fun v -> v < b.version), None)
        | Some _ -> ((fun v -> v <= b.version), None)
        | None -> ((fun _ -> false), Some b))
  in
  let highest = List.fold_left (fun h (v, _, _) -> max h v) 0 recorded in
  let to_apply =
    List.filter (fun m -> Option.is_none (recorded_as m.version)) migrations
    |> List.sort (fun a b -> Int.compare a.version b.version)
  in
  let applied_with f =
    List.filter_map
      (fun (v, _, s) ->
        match find v with
        | Some m when (not (history v)) && f m s -> Some m
        | _ -> None)
      recorded
  in
  {
    to_apply;
    unknown =
      List.filter_map
        (fun (v, _, _) ->
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

type status = {
  source : string;
  applied : (version * string) list;
  pending : migration list;
  unknown : version list;
  edited : version list;
  late : version list;
  behind : version option;
}

(* ------------------------------------------------------------------ *)
(* A dialect *)

type transaction_failure =
  [ `Not_committed of string
  | `Conflict of string option
  | `Not_serializable of string
  | `Lost of string
  | `Unsent of string ]

(* Whether a database holds the record, and which of the columns a record
   made before them may lack it has. *)
type record_columns = Absent | Present of { checksum : bool; source : bool }

(* What a database does its own way, and nothing else: the record's
   statements are rowtype's, [$1] on every dialect, and so is the reading
   of a record against a list. *)
module type Dialect = sig
  type conn

  val run : conn -> ('p, 'r) S.statement -> 'p -> ('r, [> S.error ]) result
  val exec_raw : conn -> string -> (unit, [> S.error ]) result

  val within :
    conn ->
    (conn -> ('a, ([> transaction_failure ] as 'e)) result) ->
    ('a, 'e) result

  (* One transaction, which takes the write lock as it begins where the
      dialect lets it choose. *)
  val exclusively :
    lock:int ->
    conn ->
    (holding:(conn -> (unit, [ `Lock of [ `Lost ] | S.error ]) result) ->
    (unit, error) result) ->
    (unit, error) result

  (* The run, with whatever lock the dialect takes over all of it, and
      [holding], asked at the start of each migration's transaction, which
      says whether the run still holds it. *)
  val prepare_record : table:string -> conn -> (unit, [> S.error ]) result
  (* The record's table made, and one made before brought up to it. *)

  val record_columns :
    table:string -> conn -> (record_columns, [> S.error ]) result
  (* Read, and nothing written. *)

  type mark

  val mark : conn -> (mark, [> S.error ]) result
  (* Inside a migration's transaction, before its file runs. *)

  val unbroken : conn -> mark -> (bool, [> S.error ]) result

  (* After the file ran: whether the transaction {!mark} was taken in is
      still the one open, which a file that ended it -- a [commit], a
      [rollback] -- makes false. *)
  val reset : conn -> (unit, [> S.error ]) result
  (* What a migration may set for its session put back, after each. *)
end

module Make (D : Dialect) = struct
  let applied ~table ~source conn =
    D.run conn
      (S.list ~params:S.text
         ~row:(S.t3 S.int S.text (S.opt S.text))
         (Printf.sprintf
            "select version, name, checksum from %s where source = $1 order by \
             version"
            table))
      source

  let record ~table ~source conn m =
    D.run conn
      (S.exec
         ~params:(S.t4 S.text S.int S.text S.text)
         (Printf.sprintf
            "insert into %s (source, version, name, checksum) values ($1, $2, \
             $3, $4)"
            table))
      (source, m.version, m.name, sum m)

  let recorded ~table ~source conn m =
    D.run conn
      (S.find ~params:(S.t2 S.text S.int) ~row:S.int
         (Printf.sprintf
            "select count(*) from %s where source = $1 and version = $2" table))
      (source, m.version)
    |> Result.map (fun n -> n > 0)

  let unrecord ~table ~source conn m =
    D.run conn
      (S.exec ~params:(S.t2 S.text S.int)
         (Printf.sprintf "delete from %s where source = $1 and version = $2"
            table))
      (source, m.version)

  (* One transaction per file, with the row that records it, so a failure
     leaves the database at exactly the version before it. Run once, with
     no retry, since a file run twice is a schema changed twice; and asked
     first whether it is recorded already, by another process that migrated
     while this one waited for the lock.

     A file that ends that transaction itself -- a [commit], a [rollback]
     -- runs the rest of it outside one, and a [commit] there has kept the
     record: it is refused, and its record removed.

     What a dialect will not run inside a transaction runs on its own, and
     its row is written after it: a run cut off between the two runs it
     again, which is why such a file has to be one that can run twice. *)
  let apply_migration ~holding ~table ~source conn m =
    let ended =
      "it ends the transaction it runs in, so it was not applied as one: its \
       record is removed, and what ran before that end stays as the end left \
       it. A file that runs outside a transaction says so on its first line, \
       `-- rowtype-migrate: no transaction`"
    in
    (* The record outlives the rollback only where the file committed it
       before the transaction failed. *)
    let failed_after e =
      match recorded ~table ~source conn m with
      | Ok true ->
          let* () = failed m (unrecord ~table ~source conn m) in
          Error
            (`Migration_failed
               (m, `Db (ended ^ ". It then failed: " ^ S.error_to_string e)))
      | Ok false | Error _ -> Error (`Migration_failed (m, e))
    in
    (* The row first, since in one transaction the order is not seen, and a
       migration that moves the search path -- a baseline does -- would
       otherwise leave the record's table out of it. *)
    let in_transaction () =
      match
        D.within conn (fun conn ->
            let* () =
              match holding conn with
              | Ok () -> Ok ()
              | Error (`Lock `Lost) -> Error (`Lock `Lost)
              | Error (#S.error as e) -> Error e
            in
            let* already = recorded ~table ~source conn m in
            if already then Ok ()
            else
              let* () = record ~table ~source conn m in
              let* mark = D.mark conn in
              let* () = D.exec_raw conn m.sql in
              let* unbroken = D.unbroken conn mark in
              if unbroken then Ok () else Error `Ended)
      with
      | Ok () -> Ok ()
      | Error (`Lock `Lost) -> Error (`Lock `Lost)
      | Error `Ended ->
          let* () = failed m (unrecord ~table ~source conn m) in
          Error (`Migration_failed (m, `Db ended))
      | Error (`Not_committed detail) -> failed_after (`Db detail)
      | Error (#S.error as e) -> failed_after e
    in
    let applied =
      match m.kind with
      | Transaction | Baseline -> in_transaction ()
      | No_transaction ->
          let* () =
            match holding conn with
            | Ok () -> Ok ()
            | Error (`Lock `Lost) -> Error (`Lock `Lost)
            | Error (#S.error as e) -> Error e
          in
          let* already = failed m (recorded ~table ~source conn m) in
          if already then Ok ()
          else
            failed m
              (let* () = D.exec_raw conn m.sql in
               record ~table ~source conn m)
    in
    (* What a migration sets for its session outlives it, and would carry
       into every later migration on the connection, so a schema would
       depend on how its migrations were grouped into runs. Put back after
       each; the migration's failure is the answer over the reset's. *)
    let reset = D.reset conn in
    let* () = applied in
    reset

  (* One source's migrations against its own rows of the record: every
     check is made within the source, so a library's migration is never late
     beside an application's. *)
  let pending ~holding ~table ~source conn migrations =
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
              D.run conn
                (S.exec ~params:(S.t3 S.text S.text S.int)
                   (Printf.sprintf
                      "update %s set checksum = $1 where source = $2 and \
                       version = $3"
                      table))
                (sum m, source, m.version))
            (Ok ()) r.unsummed
        in
        List.fold_left
          (fun acc m ->
            let* () = acc in
            apply_migration ~holding ~table ~source conn m)
          (Ok ()) r.to_apply

  (* The walk answers the closed type; what leaves is opened, so a caller's
     own failures can join it. *)
  let widen = function Ok v -> Ok v | Error (#error as e) -> Error e

  let apply_closed ~lock ~table ~libraries conn migrations :
      (unit, error) result =
    let* table = table_name table in
    let* () = distinct libraries in
    D.exclusively ~lock conn (fun ~holding ->
        let* () = D.prepare_record ~table conn in
        List.fold_left
          (fun acc (source, migrations) ->
            let* () = acc in
            pending ~holding ~table ~source conn migrations)
          (Ok ())
          (sources libraries migrations))

  (* A read, so it creates nothing: a database never migrated has no table,
     and that is every migration pending; one from before the source column
     holds the application's history alone, and one from before the
     checksum has no sums to compare. *)
  let reading ~table ~source conn migrations =
    let* () = listed migrations in
    let* columns = D.record_columns ~table conn in
    let* applied =
      match columns with
      | Absent -> Ok []
      | Present { source = true; _ } ->
          D.run conn
            (S.list ~params:S.text
               ~row:(S.t3 S.int S.text (S.opt S.text))
               (Printf.sprintf
                  "select version, name, checksum from %s where source = $1 \
                   order by version"
                  table))
            source
      | Present { source = false; _ } when not (String.equal source app) ->
          Ok []
      | Present { checksum = true; source = false } ->
          D.run conn
            (S.list ~params:S.unit
               ~row:(S.t3 S.int S.text (S.opt S.text))
               (Printf.sprintf
                  "select version, name, checksum from %s order by version"
                  table))
            ()
      | Present { checksum = false; source = false } ->
          Result.map
            (List.map (fun (v, n) -> (v, n, None)))
            (D.run conn
               (S.list ~params:S.unit ~row:(S.t2 S.int S.text)
                  (Printf.sprintf
                     "select version, name from %s order by version" table))
               ())
    in
    Ok (applied, read migrations applied)

  let apply ~lock ~table ~libraries conn migrations =
    widen (apply_closed ~lock ~table ~libraries conn migrations)

  let status_closed ~table ~libraries conn migrations :
      (status list, error) result =
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

  let status ~table ~libraries conn migrations =
    widen (status_closed ~table ~libraries conn migrations)
end

(* ------------------------------------------------------------------ *)
(* A history, as one file *)

let made_more_than_tables =
  `Invalid_argument
    "a library's migrations made something other than tables, which a baseline \
     cannot tell from the application's own: squash its history without those \
     libraries"

let squash ~through ~migrations ~dumped =
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

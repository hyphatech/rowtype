(* Migrations on SQLite: rowtype-migrate's record over SQLite's own ways
   -- each migration's transaction taking the write lock as it begins, a
   savepoint that says whether a file ended it, foreign keys enforced again
   after each, and a schema read from SQLite's own table. *)

module S = Rowtype
module M = Rowtype_migrate
module Sq = Rowtype_sqlite

let ( let* ) = Result.bind

module Sqlite_dialect = struct
  type conn = Sq.conn

  let run conn statement args = Sq.run conn statement args
  let exec_raw conn sql = Sq.exec_raw conn sql

  (* A migration's transaction takes the write lock as it begins, so a
     second process waits for the first to commit, and then reads what it
     recorded. *)
  let within conn work = Sq.Transaction.within ~behaviour:Immediate conn work

  (* SQLite has no lock over a session: each migration's transaction is
     what a second process waits on. *)
  let exclusively ~lock:_ _ f = f ~holding:(fun _ -> Ok ())

  (* No record made before its columns exists in SQLite, so the table is
     made whole, or is there. *)
  let prepare_record ~table conn =
    Sq.exec_raw conn
      (Printf.sprintf
         "create table if not exists %s (\n\
         \  source text not null default '%s',\n\
         \  version integer not null,\n\
         \  name text not null,\n\
         \  applied_at text not null default (strftime('%%Y-%%m-%%d \
          %%H:%%M:%%f', 'now')),\n\
         \  checksum text,\n\
         \  primary key (source, version)\n\
          )"
         table M.app)

  (* A name with a schema is a table of an attached database, whose own
     schema table lists it. *)
  let record_columns ~table conn =
    let schema, name =
      match String.split_on_char '.' table with
      | [ schema; name ] -> (schema, name)
      | _ -> ("main", table)
    in
    let* n =
      Sq.run conn
        (S.find ~params:S.text ~row:S.int
           (Printf.sprintf
              "select count(*) from \"%s\".sqlite_schema where type = 'table' \
               and name = $1"
              schema))
        name
    in
    Ok
      (if n > 0 then M.Present { checksum = true; source = true } else M.Absent)

  (* A savepoint lives as long as the transaction it was made in: a file
     that ended that transaction -- a [commit], a [rollback], or one and a
     [begin] after it -- leaves none to release. *)
  type mark = unit

  let savepoint = "rowtype_migration"
  let mark conn = Sq.exec_raw conn ("savepoint " ^ savepoint)

  let unbroken conn () =
    match Sq.exec_raw conn ("release " ^ savepoint) with
    | Ok () -> Ok true
    | Error (`Db m) when String.starts_with ~prefix:"no such savepoint" m ->
        Ok false
    | Error e -> Error e

  (* A table rebuilt with foreign keys off -- SQLite's way to change one --
     is a migration outside a transaction, where the pragma has effect, and
     the next migration is not to inherit it. *)
  let reset conn = Sq.exec_raw conn "pragma foreign_keys = on"
end

module On_sqlite = M.Make (Sqlite_dialect)

(* SQLite has no lock over a run, so there is no number to name one. *)
let apply ?(table = M.default_table) ?(libraries = []) conn migrations =
  On_sqlite.apply ~lock:0 ~table ~libraries conn migrations

let status ?(table = M.default_table) ?(libraries = []) conn migrations =
  On_sqlite.status ~table ~libraries conn migrations

(* ------------------------------------------------------------------ *)
(* A database *)

(* A SQLite file is made by opening it, so one not there is said missing
   here, before SQLite would make it; one in memory is always there. *)
let in_memory file = String.equal file ":memory:" || String.equal file ""

let with_file ~sw ~mono_clock file f =
  if (not (in_memory file)) && not (Sys.file_exists file) then
    Error (`No_database file)
  else
    let* conn = Sq.connect ~sw ~mono_clock file in
    Fun.protect ~finally:(fun () -> Sq.close conn) (fun () -> f conn)

let run env file f =
  Eio.Switch.run @@ fun sw -> with_file ~sw ~mono_clock:env#mono_clock file f

(* The files SQLite keeps beside a database while it is open. *)
let beside file = [ file ^ "-journal"; file ^ "-wal"; file ^ "-shm" ]

let create ~sw ~mono_clock file =
  if in_memory file then
    Error (`Invalid_argument "a database in memory is made by opening it")
  else if Sys.file_exists file then
    Error (`Db (Printf.sprintf "the database %s exists already" file))
  else
    let* conn = Sq.connect ~sw ~mono_clock file in
    Sq.close conn;
    Ok ()

(* A file is removed with the files SQLite keeps beside it. *)
let drop file =
  List.fold_left
    (fun acc path ->
      let* () = acc in
      if Sys.file_exists path then
        match Sys.remove path with
        | () -> Ok ()
        | exception Sys_error m -> Error (`Files m)
      else Ok ())
    (Ok ()) (file :: beside file)

(* ------------------------------------------------------------------ *)
(* The schema, as one file *)

(* A name inside a statement, which cannot be a parameter: quoted, its
   quotes doubled, it is any name SQLite keeps. *)
let quoted name =
  "\"" ^ String.concat "\"\"" (String.split_on_char '"' name) ^ "\""

(* Text inside a literal of a statement, its quotes doubled. *)
let literal text = String.concat "''" (String.split_on_char '\'' text)

(* SQLite keeps each object's statement as it was written, brought up to
   date by every [alter table]: the schema is those statements, the tables
   first, then the views, the indexes and the triggers, each kind by name,
   leaving out SQLite's own and what [left_out] names, with the tables'
   rows where they are asked for -- after the tables and before anything a
   row would set off -- each written by [quote], SQLite's own literals. *)
let sqlite_schema ~left_out ~rows conn =
  let* objects =
    Sq.run conn
      (S.list ~params:S.unit
         ~row:(S.t4 S.text S.text S.text S.text)
         "select type, name, tbl_name, sql from sqlite_schema where sql is not \
          null and name not like 'sqlite_%' order by case type when 'table' \
          then 0 when 'view' then 1 when 'index' then 2 else 3 end, name")
      ()
  in
  let objects =
    List.filter (fun (_, _, table, _) -> not (List.mem table left_out)) objects
  in
  let statement (_, _, _, sql) = sql ^ ";\n" in
  let tables, others =
    List.partition (fun (kind, _, _, _) -> String.equal kind "table") objects
  in
  let* inserts =
    if not rows then Ok []
    else
      List.fold_left
        (fun acc (_, name, _, _) ->
          let* acc = acc in
          let* columns =
            Sq.run conn
              (S.list ~params:S.text ~row:S.text
                 "select name from pragma_table_info($1) order by cid")
              name
          in
          let* lines =
            Sq.run conn
              (S.list ~params:S.unit ~row:S.text
                 (Printf.sprintf
                    "select 'insert into %s values (' || %s || ');' from %s \
                     order by %s"
                    (literal (quoted name))
                    (String.concat " || ', ' || "
                       (List.map (fun c -> "quote(" ^ quoted c ^ ")") columns))
                    (quoted name)
                    (String.concat ", " (List.map quoted columns))))
              ()
          in
          Ok (acc @ List.map (fun l -> l ^ "\n") lines))
        (Ok []) tables
  in
  Ok
    (String.concat ""
       (List.map statement tables @ inserts @ List.map statement others))

(* A library is to have made tables alone, and what is on them -- an index,
   a trigger -- goes with them. *)
let made_by_libraries ~table conn =
  let* objects =
    Sq.run conn
      (S.list ~params:S.unit ~row:(S.t2 S.text S.text)
         "select type, tbl_name from sqlite_schema where name not like \
          'sqlite_%'")
      ()
  in
  let tables =
    List.filter_map
      (fun (kind, name) ->
        if String.equal kind "table" && not (String.equal name table) then
          Some name
        else None)
      objects
  in
  if
    List.exists
      (fun (kind, on) ->
        String.equal kind "view" || not (List.mem on (table :: tables)))
      objects
  then Error M.made_more_than_tables
  else Ok tables

(* A database in memory of its own, migrated and read on one connection:
   what the migrations make from nothing. *)
let migrated ~sw ~mono_clock ~table ~libraries ~apart ~migrations f =
  let* conn = Sq.connect ~sw ~mono_clock ":memory:" in
  Fun.protect ~finally:(fun () -> Sq.close conn) @@ fun () ->
  let* theirs =
    if apart then
      let* () = On_sqlite.apply ~lock:0 ~table ~libraries conn [] in
      made_by_libraries ~table conn
    else Ok []
  in
  let* () = On_sqlite.apply ~lock:0 ~table ~libraries conn migrations in
  f conn ~theirs

let dump ~sw ~mono_clock ?(table = M.default_table) ?(libraries = []) migrations
    =
  let* table = M.table_name table in
  migrated ~sw ~mono_clock ~table ~libraries ~apart:false ~migrations
    (fun conn ~theirs:_ -> sqlite_schema ~left_out:[] ~rows:false conn)

(* The schema and every row the migrations wrote, the record and the
   libraries' tables left out, its foreign keys deferred to its commit,
   since its rows go in table by table. *)
let squash ~sw ~mono_clock ?(table = M.default_table) ?(libraries = []) ~through
    migrations =
  let* table = M.table_name table in
  M.squash ~through ~migrations ~dumped:(fun ~apart migrations ->
      migrated ~sw ~mono_clock ~table ~libraries ~apart ~migrations
        (fun conn ~theirs ->
          Result.map
            (fun schema -> "pragma defer_foreign_keys = on;\n" ^ schema)
            (sqlite_schema ~left_out:(table :: theirs) ~rows:true conn)))

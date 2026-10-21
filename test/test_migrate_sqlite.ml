(* Migrations on SQLite, the same behaviours as on Postgres, each on a file
   of the case's own: no server, so the suite always runs. *)

module S = Rowtype
module Sq = Rowtype_sqlite
module M = Rowtype_migrate
module Q = Rowtype_migrate_sqlite

let words = M.error_to_string

let ok = function
  | Ok v -> v
  | Error e -> Alcotest.failf "unexpected: %s" (words e)

let contains haystack needle =
  let n = String.length needle and h = String.length haystack in
  let rec go i =
    i + n <= h && (String.equal (String.sub haystack i n) needle || go (i + 1))
  in
  go 0

let version n =
  match M.version_of_int n with
  | Some v -> v
  | None -> Alcotest.failf "%d is not a version" n

let number (v : M.version) = (v :> int)

let m ?(kind = M.Transaction) v name sql =
  { M.version = version v; name; kind; sql }

(* A file no other case shares, and what SQLite keeps beside it, gone
   after. *)
let with_file f =
  let file = Filename.temp_file "rowtype_migrate" ".db" in
  Sys.remove file;
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun path -> if Sys.file_exists path then Sys.remove path)
        [ file; file ^ "-journal"; file ^ "-wal"; file ^ "-shm" ])
    (fun () -> f file)

let with_eio f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw -> f env ~sw

(* A database made by the library, and a connection to it. *)
let on_db f =
  with_file @@ fun file ->
  with_eio @@ fun env ~sw ->
  let mono_clock = Eio.Stdenv.mono_clock env in
  ok (Q.create ~sw ~mono_clock file);
  let conn =
    match Sq.connect ~sw ~mono_clock file with
    | Ok c -> c
    | Error e -> Alcotest.failf "connect: %s" (Sq.error_to_string e)
  in
  f conn conn

let rows conn sql =
  match
    Sq.run conn (S.list ~params:S.unit ~row:S.(t3 text int text) sql) ()
  with
  | Ok r -> r
  | Error e -> Alcotest.failf "%s" (Sq.error_to_string e)

let recorded conn =
  rows conn
    "select source, version, name from schema_migrations order by source, \
     version"

let count conn sql =
  match Sq.run conn (S.find ~params:S.unit ~row:S.int sql) () with
  | Ok n -> n
  | Error e -> Alcotest.failf "%s" (Sq.error_to_string e)

let app_status db migrations =
  match List.rev (ok (Q.status db migrations)) with
  | st :: _ -> st
  | [] -> Alcotest.fail "a status answered no source"

let first = m 20260101000000 "first" "create table a (n integer)"
let second = m 20260102000000 "second" "insert into a values (1)"
let third = m 20260103000000 "third" "create table b (n integer)"

let test_applies_and_says_what_is_pending () =
  on_db (fun db conn ->
      let st = app_status db [ first; second ] in
      Alcotest.(check (list int))
        "everything pending"
        [ 20260101000000; 20260102000000 ]
        (List.map (fun (p : M.migration) -> number p.version) st.pending);
      Alcotest.(check int)
        "a status writes nothing" 0
        (count conn "select count(*) from sqlite_schema");
      ok (Q.apply db [ first; second ]);
      Alcotest.(check (list (triple string int string)))
        "recorded, each its source's"
        [ ("app", 20260101000000, "first"); ("app", 20260102000000, "second") ]
        (recorded conn);
      ok (Q.apply db [ first; second ]);
      Alcotest.(check int) "run once" 1 (count conn "select count(*) from a");
      let st = app_status db [ first; second; third ] in
      Alcotest.(check (list int))
        "the new one pending" [ 20260103000000 ]
        (List.map (fun (p : M.migration) -> number p.version) st.pending))

let test_held_to_what_ran () =
  on_db (fun db _ ->
      ok (Q.apply db [ first; third ]);
      (match
         Q.apply db [ { first with sql = "create table a (n text)" }; third ]
       with
      | Error (`Record (`Edited e)) ->
          Alcotest.(check int) "names it" 20260101000000 (number e.version)
      | _ -> Alcotest.fail "an edited migration was not refused");
      (match Q.apply db [ first ] with
      | Error (`Record (`Unknown v)) ->
          Alcotest.(check int) "names it" 20260103000000 (number v)
      | _ -> Alcotest.fail "a newer checkout's record was not refused");
      match Q.apply db [ first; second; third ] with
      | Error (`Record (`Late l)) ->
          Alcotest.(check int) "names it" 20260102000000 (number l.version)
      | _ -> Alcotest.fail "a late migration was not refused")

let test_a_failed_migration_stops_where_it_failed () =
  on_db (fun db conn ->
      let broken =
        m 20260102000000 "broken"
          "insert into a values (1); insert into nope values (1)"
      in
      (match Q.apply db [ first; broken; third ] with
      | Error (`Migration_failed (f, _)) ->
          Alcotest.(check int) "names it" 20260102000000 (number f.version)
      | _ -> Alcotest.fail "a broken migration was not refused");
      Alcotest.(check (list (triple string int string)))
        "at the version before it"
        [ ("app", 20260101000000, "first") ]
        (recorded conn);
      Alcotest.(check int)
        "nothing of it kept" 0
        (count conn "select count(*) from a"))

let test_a_migration_that_ends_its_transaction_is_refused () =
  List.iter
    (fun ending ->
      on_db (fun db conn ->
          let ends =
            m 20260102000000 "ends" ("insert into a values (1); " ^ ending)
          in
          (match Q.apply db [ first; ends ] with
          | Error (`Migration_failed (_, e)) ->
              Alcotest.(check bool)
                (ending ^ ": says why") true
                (contains (S.error_to_string e) "ends the transaction")
          | _ -> Alcotest.failf "%s: not refused" ending);
          Alcotest.(check (list (triple string int string)))
            (ending ^ ": its record removed")
            [ ("app", 20260101000000, "first") ]
            (recorded conn)))
    [ "commit"; "rollback"; "commit; begin" ]

(* [vacuum] runs only outside a transaction, which a file says on its first
   line; and a table rebuilt with foreign keys off leaves them on after. *)
let test_a_migration_can_run_outside_a_transaction () =
  on_db (fun db conn ->
      let vacuum kind =
        m ~kind 20260102000000 "vacuum"
          ((match kind with
             | M.No_transaction -> "-- rowtype-migrate: no transaction\n"
             | M.Transaction | M.Baseline -> "")
          ^ "vacuum")
      in
      Alcotest.(check bool)
        "refused inside one" false
        (Result.is_ok (Q.apply db [ first; vacuum Transaction ]));
      ok (Q.apply db [ first; vacuum No_transaction ]);
      let off =
        m ~kind:No_transaction 20260103000000 "off"
          "-- rowtype-migrate: no transaction\npragma foreign_keys = off"
      in
      ok (Q.apply db [ first; vacuum No_transaction; off ]);
      Alcotest.(check int)
        "foreign keys on again" 1
        (count conn "pragma foreign_keys"))

let test_a_baseline_stands_for_what_it_replaced () =
  let baseline =
    m ~kind:Baseline 20260102000000 "baseline"
      "-- rowtype-migrate: baseline\n\
       create table a (n integer);\n\
       insert into a values (1);"
  in
  on_db (fun db conn ->
      ok (Q.apply db [ first; second ]);
      ok (Q.apply db [ baseline; third ]);
      Alcotest.(check (list (triple string int string)))
        "history kept, the next applied"
        [
          ("app", 20260101000000, "first");
          ("app", 20260102000000, "second");
          ("app", 20260103000000, "third");
        ]
        (recorded conn));
  (* A migration may be named anything, baseline among them: what it ran
     below the baseline's version says the baseline did not run there. *)
  on_db (fun db conn ->
      ok (Q.apply db [ first; { second with name = "baseline" } ]);
      ok (Q.apply db [ baseline; third ]);
      Alcotest.(check int)
        "one named baseline is history too" 3
        (List.length (recorded conn)));
  (* A database made from the baseline ran the baseline, so its text is held
     to what ran, as any migration's is. *)
  on_db (fun db _ ->
      ok (Q.apply db [ baseline; third ]);
      let edited =
        { baseline with sql = baseline.sql ^ "\ncreate table d (n integer);" }
      in
      match Q.apply db [ edited; third ] with
      | Error (`Record (`Edited (m : M.migration))) ->
          Alcotest.(check string) "the baseline, edited" "baseline" m.name
      | Ok () -> Alcotest.fail "an edited baseline was accepted"
      | Error e -> Alcotest.failf "not an edit: %s" (M.error_to_string e))

let test_a_record_can_have_a_table_of_its_own () =
  on_db (fun db conn ->
      ok (Q.apply ~table:"migrations" db [ first ]);
      Alcotest.(check int)
        "in its table" 1
        (count conn "select count(*) from migrations");
      Alcotest.(check int)
        "and not the default" 0
        (count conn
           "select count(*) from sqlite_schema where name = 'schema_migrations'");
      match Q.apply ~table:"m; drop table a" db [ first ] with
      | Error (`Invalid_argument _) -> ()
      | _ -> Alcotest.fail "a name outside the grammar was written")

let test_a_database_is_made_and_dropped () =
  with_file @@ fun file ->
  with_eio @@ fun env ~sw ->
  let mono_clock = Eio.Stdenv.mono_clock env in
  let connects () = Q.run env file (fun _ -> Ok ()) in
  (match connects () with
  | Error (`No_database missing) ->
      Alcotest.(check string) "names it" file missing
  | Ok () -> Alcotest.fail "a database there before it was made"
  | Error e -> Alcotest.failf "not said missing: %s" (words e));
  Alcotest.(check bool) "not made by asking" false (Sys.file_exists file);
  ok (Q.create ~sw ~mono_clock file);
  ok (connects ());
  Alcotest.(check bool)
    "made twice" false
    (Result.is_ok (Q.create ~sw ~mono_clock file));
  ok (Q.drop file);
  Alcotest.(check bool) "and gone" false (Result.is_ok (connects ()));
  ok (Q.drop file);
  (* A WAL database keeps two files beside it while a connection is open,
     which go with it. *)
  ok (Q.create ~sw ~mono_clock file);
  let conn =
    match Sq.connect ~sw ~mono_clock file with
    | Ok c -> c
    | Error e -> Alcotest.failf "%s" (Sq.error_to_string e)
  in
  List.iter
    (fun sql ->
      match Sq.exec_raw conn sql with
      | Ok () -> ()
      | Error e -> Alcotest.failf "%s" (Sq.error_to_string e))
    [ "pragma journal_mode = wal"; "create table t (n integer)" ];
  Alcotest.(check bool)
    "a -wal beside it" true
    (Sys.file_exists (file ^ "-wal"));
  ok (Q.drop file);
  Sq.close conn;
  Alcotest.(check (list string))
    "nothing of it left" []
    (List.filter Sys.file_exists [ file; file ^ "-wal"; file ^ "-shm" ])

(* Two processes migrating one file at once, each having read the record
   before either applies anything: a third holds the write lock until both
   wait at their [begin], which their observers say. Each migration's
   transaction asks the record first, so the second finds it applied, and
   nothing runs twice -- a [create table] run twice would fail. *)
let test_two_migrators_take_turns () =
  with_file @@ fun file ->
  with_eio @@ fun env ~sw ->
  let mono_clock = Eio.Stdenv.mono_clock env in
  ok (Q.create ~sw ~mono_clock file);
  let connect ?observe () =
    match Sq.connect ~sw ~mono_clock ?observe file with
    | Ok c -> c
    | Error e -> Alcotest.failf "connect: %s" (Sq.error_to_string e)
  in
  let waiting () =
    let begun, begin_ = Eio.Promise.create () in
    let observe =
      {
        Sq.around =
          (fun sql run ->
            if String.starts_with ~prefix:"begin" sql then
              ignore (Eio.Promise.try_resolve begin_ () : bool);
            run ());
      }
    in
    (connect ~observe (), begun)
  in
  let holder = connect () in
  ok (Q.apply holder []);
  (match Sq.exec_raw holder "begin immediate" with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%s" (Sq.error_to_string e));
  let a, a_waits = waiting () and b, b_waits = waiting () in
  let migrations =
    [
      m 20260101000000 "once" "create table once (n integer)";
      m 20260102000000 "after" "create table after (n integer)";
    ]
  in
  let answers = ref [] in
  Eio.Fiber.all
    [
      (fun () -> answers := Q.apply a migrations :: !answers);
      (fun () -> answers := Q.apply b migrations :: !answers);
      (fun () ->
        Eio.Promise.await a_waits;
        Eio.Promise.await b_waits;
        match Sq.exec_raw holder "rollback" with
        | Ok () -> ()
        | Error e -> Alcotest.failf "%s" (Sq.error_to_string e));
    ];
  List.iter ok !answers;
  Alcotest.(check int)
    "each recorded once" 2
    (count a "select count(*) from schema_migrations")

(* A status says each way a list and a record disagree, as [apply] refuses
   them, and writes nothing. *)
let test_a_status_says_each_disagreement () =
  on_db (fun db conn ->
      ok (Q.apply db [ first; third ]);
      let versions = List.map number
      and named = List.map (fun (m : M.migration) -> number m.version) in
      let st =
        app_status db
          [ { first with sql = "create table a (n text)" }; second; third ]
      in
      Alcotest.(check (list int)) "edited" [ 20260101000000 ] (named st.edited);
      Alcotest.(check (list int)) "late" [ 20260102000000 ] (named st.late);
      Alcotest.(check (option string))
        "refused as apply refuses it, the edited one first"
        (Some
           (M.error_to_string
              (`Record (`Edited { first with sql = "create table a (n text)" }))))
        (Option.map M.error_to_string (M.refusal st));
      let st = app_status db [ first ] in
      Alcotest.(check (list int))
        "unknown" [ 20260103000000 ] (versions st.unknown);
      Alcotest.(check (list (triple string int string)))
        "and nothing written"
        [ ("app", 20260101000000, "first"); ("app", 20260103000000, "third") ]
        (recorded conn))

(* A database whose record stops before a baseline is behind the squash:
   said by a status, and refused by [apply] with nothing applied. *)
let test_a_database_behind_a_baseline () =
  on_db (fun db conn ->
      ok (Q.apply db [ first ]);
      let baseline =
        m ~kind:Baseline 20260102000000 "baseline"
          "-- rowtype-migrate: baseline\ncreate table a (n integer);"
      in
      let st = app_status db [ baseline; third ] in
      Alcotest.(check (option int))
        "said" (Some 20260102000000)
        (Option.map (fun (m : M.migration) -> number m.version) st.behind);
      (match Q.apply db [ baseline; third ] with
      | Error (`Record (`Behind b)) ->
          Alcotest.(check int) "names it" 20260102000000 (number b.version)
      | _ -> Alcotest.fail "a database behind the squash was migrated");
      Alcotest.(check int)
        "nothing applied" 0
        (count conn "select count(*) from sqlite_schema where name = 'b'"))

(* A record in an attached database is found and written there. *)
let test_a_record_in_an_attached_database () =
  on_db (fun db conn ->
      (match Sq.exec_raw conn "attach ':memory:' as aux" with
      | Ok () -> ()
      | Error e -> Alcotest.failf "%s" (Sq.error_to_string e));
      Alcotest.(check int)
        "pending, with no record yet" 1
        (List.length
           (match
              List.rev (ok (Q.status ~table:"aux.migrations" db [ first ]))
            with
           | st :: _ -> st.pending
           | [] -> []));
      ok (Q.apply ~table:"aux.migrations" db [ first ]);
      Alcotest.(check int)
        "recorded there" 1
        (count conn "select count(*) from aux.migrations");
      ok (Q.apply ~table:"aux.migrations" db [ first ]);
      Alcotest.(check int)
        "and read there" 1
        (count conn "select count(*) from aux.migrations"))

let library name migrations =
  match M.source name migrations with
  | Ok l -> l
  | Error e -> Alcotest.failf "no source %s: %s" name (words e)

let test_libraries_are_histories_of_their_own () =
  on_db (fun db conn ->
      let app =
        [ m 20260201000000 "orders" "create table orders (n integer)" ]
      in
      ok (Q.apply db app);
      let spool =
        library "spool"
          [ m 20260101000000 "jobs" "create table spool_jobs (n integer)" ]
      in
      ok (Q.apply ~libraries:[ spool ] db app);
      Alcotest.(check (list (triple string int string)))
        "each row its source's"
        [ ("app", 20260201000000, "orders"); ("spool", 20260101000000, "jobs") ]
        (recorded conn);
      Alcotest.(check (list string))
        "a status for each, the application's last" [ "spool"; "app" ]
        (List.map
           (fun (st : M.status) -> st.source)
           (ok (Q.status ~libraries:[ spool ] db app))))

(* Every check is made within one source, so a library's own history is
   held to its order as the application's is. *)
let test_a_source_is_still_held_to_its_order () =
  on_db (fun db _ ->
      let later = m 20260102000000 "later" "select 1" in
      ok (Q.apply ~libraries:[ library "spool" [ later ] ] db []);
      let earlier = m 20260101000000 "earlier" "select 1" in
      match Q.apply ~libraries:[ library "spool" [ earlier; later ] ] db [] with
      | Error (`Record (`Late (m : M.migration))) ->
          Alcotest.(check string) "the late one" "earlier" m.name
      | Ok () -> Alcotest.fail "a late migration within a source ran"
      | Error e -> Alcotest.failf "refused otherwise: %s" (words e))

(* The libraries run first, so the application's migrations may stand on
   their tables -- SQLite takes a foreign key to a table not yet made, so
   a row written into one says it -- and a source given twice is refused. *)
let test_libraries_run_before_the_application () =
  on_db (fun db _ ->
      let spool =
        library "spool"
          [ m 20260301000000 "jobs" "create table spool_jobs (id integer)" ]
      in
      ok
        (Q.apply ~libraries:[ spool ] db
           [ m 20260101000000 "first job" "insert into spool_jobs values (1)" ]);
      match Q.apply ~libraries:[ spool; spool ] db [] with
      | Error (`Invalid_argument _) -> ()
      | Ok () | Error _ -> Alcotest.fail "a source given twice was taken")

(* ------------------------------------------------------------------ *)
(* The schema, and a history as one file *)

let schema_of env ~sw ?libraries migrations =
  Q.dump ~sw ~mono_clock:(Eio.Stdenv.mono_clock env) ?libraries migrations

let test_a_dump_is_what_the_migrations_make () =
  with_eio @@ fun env ~sw ->
  let migrations =
    [
      m 20260101000000 "a" "create table a (n integer)";
      m 20260102000000 "index" "create index a_n on a (n)";
      m 20260103000000 "column" "alter table a add column note text";
    ]
  in
  let schema = ok (schema_of env ~sw migrations) in
  Alcotest.(check string)
    "the same on every run" schema
    (ok (schema_of env ~sw migrations));
  List.iter
    (fun part ->
      if not (contains schema part) then
        Alcotest.failf "no %S in %S" part schema)
    [
      "CREATE TABLE a (n integer, note text);";
      "CREATE INDEX a_n on a (n);";
      "CREATE TABLE schema_migrations";
    ];
  Alcotest.(check bool)
    "a file is not touched" false
    (Sys.file_exists "unused.db")

let squash env ~sw ?libraries ~through migrations =
  Q.squash ~sw
    ~mono_clock:(Eio.Stdenv.mono_clock env)
    ?libraries ~through:(version through) migrations

let test_a_squash_stands_for_its_history () =
  with_eio @@ fun env ~sw ->
  let later = m 20260104000000 "later" "create table later (n integer)" in
  let history =
    [
      m 20260101000000 "parents" "create table parents (id integer primary key)";
      m 20260102000000 "children"
        "create table children (parent integer references parents (id)); \
         create trigger counted after insert on children begin insert into \
         parents values ((select max(id) + 100 from parents)); end";
      m 20260103000000 "seeded"
        "insert into parents values (1); insert into children values (1)";
      later;
    ]
  in
  let baseline = ok (squash env ~sw ~through:20260103000000 history) in
  Alcotest.(check int)
    "at the version it replaced" 20260103000000 (number baseline.version);
  Alcotest.(check bool)
    "a baseline" true
    (match baseline.kind with
    | Baseline -> true
    | Transaction | No_transaction -> false);
  List.iter
    (fun part ->
      if not (contains baseline.sql part) then
        Alcotest.failf "no %S in the baseline" part)
    [
      "pragma defer_foreign_keys = on;";
      "insert into \"children\" values (1);";
      "insert into \"parents\" values (101);";
      "CREATE TRIGGER counted";
    ];
  Alcotest.(check bool)
    "no record in it" false
    (contains baseline.sql "schema_migrations");
  (* Applied to a database of its own, the baseline and what follows make
     what the history made: the squash proved it, and so does running it. *)
  on_db (fun db conn ->
      ok (Q.apply db [ baseline; later ]);
      Alcotest.(check int)
        "rows, the trigger's included, and no trigger fired again" 2
        (count conn "select count(*) from parents"));
  match squash env ~sw ~through:20260105000000 history with
  | Error (`Invalid_argument _) -> ()
  | _ -> Alcotest.fail "a squash through no migration was answered"

(* A migration after the baseline whose rows differ on every run makes two
   databases that cannot dump the same: nothing is squashed. *)
let test_an_unproved_squash_is_refused () =
  with_eio @@ fun env ~sw ->
  match
    squash env ~sw ~through:20260101000000
      [
        m 20260101000000 "a" "create table a (n real)";
        m 20260102000000 "chance" "insert into a select random()";
      ]
  with
  | Error `Unproved -> ()
  | Ok _ -> Alcotest.fail "a squash that could not be proved was answered"
  | Error e -> Alcotest.failf "refused otherwise: %s" (words e)

let test_a_squash_leaves_a_library_out () =
  with_eio @@ fun env ~sw ->
  let spool =
    library "spool"
      [ m 20250101000000 "jobs" "create table spool_jobs (n integer)" ]
  in
  let history =
    [
      m 20260101000000 "a" "create table a (n integer)";
      m 20260102000000 "b" "create table b (n integer)";
    ]
  in
  let baseline =
    ok (squash env ~sw ~libraries:[ spool ] ~through:20260102000000 history)
  in
  Alcotest.(check bool)
    "the library's table left out" false
    (contains baseline.sql "spool_jobs");
  (* A trigger on a library's table is the library's, and goes with it. *)
  let counted =
    library "spool"
      [
        m 20260101000000 "jobs" "create table spool_jobs (n integer)";
        m 20260101000001 "counted"
          "create trigger spool_counted after insert on spool_jobs begin \
           select 1; end";
      ]
  in
  let baseline =
    ok (squash env ~sw ~libraries:[ counted ] ~through:20260102000000 history)
  in
  Alcotest.(check bool)
    "the library's trigger left out with it" false
    (contains baseline.sql "spool_counted");
  let viewer =
    library "viewer" [ m 20250101000000 "view" "create view v as select 1" ]
  in
  match
    squash env ~sw ~libraries:[ viewer ] ~through:20260102000000 history
  with
  | Error (`Invalid_argument _) -> ()
  | _ -> Alcotest.fail "a library that made a view was squashed over"

let () =
  Alcotest.run "rowtype-migrate on SQLite"
    [
      ( "migrations",
        [
          Alcotest.test_case "applied, and what is pending said" `Quick
            test_applies_and_says_what_is_pending;
          Alcotest.test_case "held to what ran" `Quick test_held_to_what_ran;
          Alcotest.test_case "a failed migration stops where it failed" `Quick
            test_a_failed_migration_stops_where_it_failed;
          Alcotest.test_case "a migration that ends its transaction is refused"
            `Quick test_a_migration_that_ends_its_transaction_is_refused;
          Alcotest.test_case "a migration can run outside a transaction" `Quick
            test_a_migration_can_run_outside_a_transaction;
          Alcotest.test_case "a baseline stands for what it replaced" `Quick
            test_a_baseline_stands_for_what_it_replaced;
          Alcotest.test_case "a record can have a table of its own" `Quick
            test_a_record_can_have_a_table_of_its_own;
          Alcotest.test_case "two migrators take turns" `Quick
            test_two_migrators_take_turns;
          Alcotest.test_case "libraries are histories of their own" `Quick
            test_libraries_are_histories_of_their_own;
          Alcotest.test_case "a source is still held to its order" `Quick
            test_a_source_is_still_held_to_its_order;
          Alcotest.test_case "libraries run before the application" `Quick
            test_libraries_run_before_the_application;
          Alcotest.test_case "a status says each disagreement" `Quick
            test_a_status_says_each_disagreement;
          Alcotest.test_case "a database behind a baseline" `Quick
            test_a_database_behind_a_baseline;
          Alcotest.test_case "a record in an attached database" `Quick
            test_a_record_in_an_attached_database;
        ] );
      ( "the database",
        [
          Alcotest.test_case "made and dropped" `Quick
            test_a_database_is_made_and_dropped;
        ] );
      ( "one file",
        [
          Alcotest.test_case "a dump is what the migrations make" `Quick
            test_a_dump_is_what_the_migrations_make;
          Alcotest.test_case "a squash stands for its history" `Quick
            test_a_squash_stands_for_its_history;
          Alcotest.test_case "an unproved squash is refused" `Quick
            test_an_unproved_squash_is_refused;
          Alcotest.test_case "a squash leaves a library out" `Quick
            test_a_squash_leaves_a_library_out;
        ] );
    ]

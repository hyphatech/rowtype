module S = Rowtype
module Pg = Rowtype_postgres

let ok = function
  | Ok v -> v
  | Error e -> Alcotest.failf "unexpected error: %s" (S.error_to_string e)

let ok_s = function Ok v -> v | Error m -> Alcotest.failf "unexpected: %s" m

(* Each case gets a database of its own. See [db_target.ml]. *)
let on_db setup f =
  Db_target.with_postgres (fun target ->
      let db = Db_target.connect target in
      Fun.protect
        ~finally:(fun () -> Pg.close db)
        (fun () ->
          List.iter (fun s -> ok (Pg.exec_raw db s)) setup;
          f db))

(* A connection bounded as a server bounds its own, so a migration has a
   bound to lift. *)
let connect ?statement_timeout_ms target =
  let parameters =
    match statement_timeout_ms with
    | None -> []
    | Some ms -> [ ("statement_timeout", string_of_int ms) ]
  in
  ok
    (Result.bind (Pg.conninfo target)
       (Pg.connect ~sw:(Db_target.sw ()) ~net:(Db_target.net ())
          ~mono_clock:(Db_target.mono ()) ~parameters))

let contains haystack needle =
  let n = String.length needle and h = String.length haystack in
  let rec go i =
    i + n <= h && (String.equal (String.sub haystack i n) needle || go (i + 1))
  in
  go 0

(* A migration is bounded by nothing but itself, whatever the connection
   carries for requests: the server's statement bound and the driver's on
   every read, each lifted for the run and each back after it. *)
let test_a_migration_is_unbounded () =
  Db_target.with_postgres (fun target ->
      let db = connect ~statement_timeout_ms:100 target in
      Pg.set_timeout db (Some 0.2);
      Fun.protect
        ~finally:(fun () -> Pg.close db)
        (fun () ->
          (match
             Rowtype_migrate.run db
               [
                 {
                   Rowtype_migrate.version = 20260101000000;
                   name = "slow";
                   kind = Transaction;
                   sql = "select pg_sleep(0.3); create table slow (n int)";
                 };
               ]
           with
          | Ok () -> ()
          | Error m -> Alcotest.failf "the migration was cut short: %s" m);
          Alcotest.(check (option (float 0.)))
            "the read bound back" (Some 0.2) (Pg.timeout db);
          Pg.set_timeout db None;
          match Pg.exec_raw db "select pg_sleep(1)" with
          | Error _ -> ()
          | Ok () -> Alcotest.fail "the request bound did not come back"))

(* What would run, read without running it -- and without writing so much
   as the table that records it, so a status is safe against production. *)
let test_a_status_says_what_is_pending_and_writes_nothing () =
  on_db [] (fun db ->
      let m version name =
        { Rowtype_migrate.version; name; kind = Transaction; sql = "select 1" }
      in
      let first = m 20260101000000 "first"
      and second = m 20260102000000 "second" in
      let pending_of (st : Rowtype_migrate.status) =
        List.map (fun (m : Rowtype_migrate.migration) -> m.name) st.pending
      in
      let st = ok_s (Rowtype_migrate.status db [ second; first ]) in
      Alcotest.(check (list string))
        "all pending, in order" [ "first"; "second" ] (pending_of st);
      Alcotest.(check (option bool))
        "and nothing written" (Some false)
        (ok
           (Pg.run db
              (S.find_opt ~params:S.unit ~row:S.bool
                 "select to_regclass('schema_migrations') is not null")
              ()));
      ignore (ok_s (Rowtype_migrate.run db [ first ]));
      let st = ok_s (Rowtype_migrate.status db [ first; second ]) in
      Alcotest.(check (list string)) "one left" [ "second" ] (pending_of st);
      Alcotest.(check (list (pair int string)))
        "one applied"
        [ (20260101000000, "first") ]
        st.applied;
      let st = ok_s (Rowtype_migrate.status db [ second ]) in
      Alcotest.(check (list int))
        "a version this list does not know" [ 20260101000000 ] st.unknown)

(* A migration is known by its text: one edited after it ran is refused,
   one merged after a later one ran is refused, and a database migrated
   before the digests were kept has them recorded on its next run. *)
let test_a_migration_is_held_to_what_ran () =
  on_db [] (fun db ->
      let m version name sql =
        { Rowtype_migrate.version; name; kind = Transaction; sql }
      in
      let first = m 20260101000000 "first" "create table a (n int)"
      and third = m 20260103000000 "third" "create table c (n int)" in
      ignore (ok_s (Rowtype_migrate.run db [ first; third ]));
      let refused ms sub =
        match Rowtype_migrate.run db ms with
        | Error e -> contains e sub
        | Ok () -> false
      in
      Alcotest.(check bool)
        "an edited one" true
        (refused
           [ m 20260101000000 "first" "create table a (n bigint)"; third ]
           "edited");
      Alcotest.(check bool)
        "a late one" true
        (refused [ first; m 20260102000000 "second" "select 1"; third ] "older");
      let st =
        ok_s
          (Rowtype_migrate.status db
             [
               m 20260101000000 "first" "changed";
               m 20260102000000 "second" "select 1";
               third;
             ])
      in
      Alcotest.(check (pair (list int) (list int)))
        "and a status says both"
        ([ 20260101000000 ], [ 20260102000000 ])
        (st.edited, st.late);
      ok (Pg.exec_raw db "update schema_migrations set checksum = null");
      ignore (ok_s (Rowtype_migrate.run db [ first; third ]));
      Alcotest.(check (list (option string)))
        "unrecorded sums recorded"
        [ Some (Digest.to_hex (Digest.string first.sql)) ]
        (ok
           (Pg.run db
              (S.list ~params:S.int
                 ~row:S.(opt text)
                 "select checksum from schema_migrations where version = $1")
              first.version)))

(* A migration that fails leaves the database at the version before it:
   what it ran and its row rolled back, what follows it not run, said by
   its name, and the lock given up for the run that comes after the fix. *)
let test_a_failed_migration_stops_where_it_failed () =
  Db_target.with_postgres (fun target ->
      let db = Db_target.connect target and other = Db_target.connect target in
      Fun.protect
        ~finally:(fun () ->
          Pg.close db;
          Pg.close other)
        (fun () ->
          let m version name sql =
            { Rowtype_migrate.version; name; kind = Transaction; sql }
          in
          let first = m 20260101000000 "first" "create table a (n int)"
          and broken =
            m 20260102000000 "broken" "create table b (n int); selec 1"
          and third = m 20260103000000 "third" "create table c (n int)" in
          (match Rowtype_migrate.run db [ first; broken; third ] with
          | Ok () -> Alcotest.fail "a broken migration was applied"
          | Error e ->
              Alcotest.(check bool)
                ("named: " ^ e) true
                (contains e "migration 20260102000000_broken failed"));
          let st = ok_s (Rowtype_migrate.status db [ first; broken; third ]) in
          Alcotest.(check (list (pair int string)))
            "the one before it applied"
            [ (20260101000000, "first") ]
            st.applied;
          Alcotest.(check (list (option bool)))
            "nothing of it or after it" [ Some false; Some false ]
            (List.map
               (fun name ->
                 ok
                   (Pg.run db
                      (S.find_opt ~params:S.text ~row:S.bool
                         "select to_regclass($1) is not null")
                      name))
               [ "b"; "c" ]);
          Alcotest.(check bool)
            "the lock given up" true
            (ok
               (Pg.run other
                  (S.find ~params:S.int ~row:S.bool
                     "select pg_try_advisory_lock($1)")
                  Rowtype_migrate.default_lock));
          ok
            (Pg.run other
               (S.exec ~params:S.int "select pg_advisory_unlock($1)")
               Rowtype_migrate.default_lock);
          let fixed =
            [ first; m 20260102000000 "broken" "create table b (n int)"; third ]
          in
          ok_s (Rowtype_migrate.run db fixed);
          Alcotest.(check int)
            "and the fix runs" 3
            (List.length (ok_s (Rowtype_migrate.status db fixed)).applied)))

(* A migration whose connection is lost is the failure told, by its name,
   rather than the lock's release that cannot follow it. *)
let test_a_lost_connection_names_its_migration () =
  on_db [] (fun db ->
      match
        Rowtype_migrate.run db
          [
            {
              Rowtype_migrate.version = 20260101000000;
              name = "lost";
              kind = Transaction;
              sql = "select pg_terminate_backend(pg_backend_pid())";
            };
          ]
      with
      | Ok () -> Alcotest.fail "a lost connection migrated"
      | Error e ->
          Alcotest.(check bool)
            ("named: " ^ e) true
            (contains e "migration 20260101000000_lost failed"))

(* What Postgres refuses inside a transaction runs on its own, and is
   recorded once it has; a run cut off before the row runs the file again,
   which is why it is written to run twice. *)
let test_a_migration_can_run_outside_a_transaction () =
  on_db [] (fun db ->
      let table =
        {
          Rowtype_migrate.version = 20260101000000;
          name = "table";
          kind = Transaction;
          sql = "create table t (n int)";
        }
      and index kind =
        {
          Rowtype_migrate.version = 20260102000000;
          name = "index";
          kind;
          sql = "create index concurrently if not exists t_n on t (n)";
        }
      in
      Alcotest.(check bool)
        "refused inside one" false
        (Result.is_ok (Rowtype_migrate.run db [ table; index Transaction ]));
      ok_s (Rowtype_migrate.run db [ table; index No_transaction ]);
      let recorded () =
        ok
          (Pg.run db
             (S.list ~params:S.unit ~row:S.int
                "select version from schema_migrations order by version")
             ())
      in
      Alcotest.(check (list int))
        "and recorded"
        [ 20260101000000; 20260102000000 ]
        (recorded ());
      ok
        (Pg.exec_raw db
           "delete from schema_migrations where version = 20260102000000");
      ok_s (Rowtype_migrate.run db [ table; index No_transaction ]);
      Alcotest.(check (list int))
        "run again where its row was lost"
        [ 20260101000000; 20260102000000 ]
        (recorded ()))

(* A baseline stands for the migrations it replaced: a database that ran
   them takes it as applied and their rows as history, a database made after
   it runs it -- in the session a pg_dump leaves, which is put back for what
   follows -- and a database behind it is refused, by all three readers. *)
let test_a_baseline_stands_for_what_it_replaced () =
  let m version name kind sql = { Rowtype_migrate.version; name; kind; sql } in
  let a = m 20260101000000 "a" Transaction "create table a (n int)"
  and b = m 20260102000000 "b" Transaction "insert into a values (1)"
  and c = m 20260103000000 "c" Transaction "create table c (n int)" in
  let baseline =
    m 20260102000000 "baseline" Baseline
      "-- rowtype-migrate: baseline\n\
       select pg_catalog.set_config('search_path', '', false);\n\
       create table public.a (n int);\n\
       insert into public.a values (1);\n"
  in
  let squashed = [ baseline; c ] in
  on_db [] (fun db ->
      ok_s (Rowtype_migrate.run db [ a; b ]);
      ok_s (Rowtype_migrate.run db squashed);
      let st = ok_s (Rowtype_migrate.status db squashed) in
      Alcotest.(check (pair (list int) (list int)))
        "what it replaced is history, not unknown" ([], [])
        (st.unknown, st.edited);
      Alcotest.(check int) "and the rest applied" 3 (List.length st.applied);
      Alcotest.(check (option int)) "and nothing behind" None st.behind);
  on_db [] (fun db ->
      ok_s (Rowtype_migrate.run db squashed);
      Alcotest.(check int)
        "made from the baseline, rows and all" 1
        (ok
           (Pg.run db
              (S.find ~params:S.unit ~row:S.int "select count(*)::int from a")
              ()));
      Alcotest.(check (list int))
        "and recorded at its version"
        [ 20260102000000; 20260103000000 ]
        (ok
           (Pg.run db
              (S.list ~params:S.unit ~row:S.int
                 "select version from schema_migrations order by version")
              ())));
  on_db [] (fun db ->
      ok_s (Rowtype_migrate.run db [ a ]);
      let says e = contains e "squash" in
      Alcotest.(check bool)
        "behind it, run refuses" true
        (match Rowtype_migrate.run db squashed with
        | Error e -> says e
        | Ok () -> false);
      Alcotest.(check (option int))
        "and a status says" (Some 20260102000000)
        (ok_s (Rowtype_migrate.status db squashed)).behind)

(* A record may be kept in a table of its own, in a schema of its own, so
   two applications can share a database; a name that would have to be
   quoted is refused, since it goes into the statements as it is. *)
let test_a_record_can_have_a_table_of_its_own () =
  on_db [ "create schema app" ] (fun db ->
      let one =
        {
          Rowtype_migrate.version = 20260101000000;
          name = "one";
          kind = Transaction;
          sql = "create table app.one (n int)";
        }
      in
      ok_s (Rowtype_migrate.run ~table:"app.migrations" db [ one ]);
      let exists name =
        ok
          (Pg.run db
             (S.find ~params:S.text ~row:S.bool
                "select to_regclass($1) is not null")
             name)
      in
      Alcotest.(check (pair bool bool))
        "kept there, and nowhere else" (true, false)
        (exists "app.migrations", exists "schema_migrations");
      let st =
        ok_s (Rowtype_migrate.status ~table:"app.migrations" db [ one ])
      in
      Alcotest.(check int) "read back from it" 1 (List.length st.applied);
      Alcotest.(check bool)
        "a name that is not a table's" false
        (Result.is_ok
           (Rowtype_migrate.run ~table:"m; drop table app.one" db [ one ])))

let on_server f url =
  f ~sw:(Db_target.sw ()) ~net:(Db_target.net ())
    ~mono_clock:(Db_target.mono ()) url

(* A database is made and dropped by its URL, and one that is not there is
   said so, with the command that makes it. *)
let test_a_database_is_made_and_dropped () =
  let name = Printf.sprintf "rowtype_migrate_made_%d" (Unix.getpid ()) in
  let url = Db_target.on_database name in
  let connects () =
    Rowtype_migrate.with_connection ~sw:(Db_target.sw ())
      ~net:(Db_target.net ()) ~mono_clock:(Db_target.mono ()) url (fun _ ->
        Ok ())
  in
  (match connects () with
  | Ok () -> Alcotest.fail "a database there before it was made"
  | Error m ->
      Alcotest.(check bool)
        ("names what makes it: " ^ m)
        true
        (contains m "rowtype-migrate create"));
  ok_s (on_server Rowtype_migrate.create url);
  Fun.protect
    ~finally:(fun () -> ignore (on_server Rowtype_migrate.drop url))
    (fun () ->
      ok_s (connects ());
      Alcotest.(check bool)
        "made twice" false
        (Result.is_ok (on_server Rowtype_migrate.create url)));
  ok_s (on_server Rowtype_migrate.drop url);
  Alcotest.(check bool) "and gone" false (Result.is_ok (connects ()));
  ok_s (on_server Rowtype_migrate.drop url)

(* Two processes migrating one database at once take turns under the
   advisory lock: both succeed, and every migration runs once. *)
let test_two_migrators_take_turns () =
  Db_target.with_postgres (fun target ->
      let a = connect target and b = connect target in
      Fun.protect
        ~finally:(fun () ->
          Pg.close a;
          Pg.close b)
        (fun () ->
          let m version name sql =
            { Rowtype_migrate.version; name; kind = Transaction; sql }
          in
          let migrations =
            [
              m 20260101000000 "slow"
                "select pg_sleep(0.2); create table once (n int)";
              m 20260102000000 "after" "insert into once values (1)";
            ]
          in
          let first, second =
            Eio.Fiber.pair
              (fun () -> Rowtype_migrate.run a migrations)
              (fun () -> Rowtype_migrate.run b migrations)
          in
          List.iter
            (function Ok () -> () | Error m -> Alcotest.failf "refused: %s" m)
            [ first; second ];
          Alcotest.(check int)
            "each migration ran once" 1
            (ok
               (Pg.run a
                  (S.find ~params:S.unit ~row:S.int "select count(*) from once")
                  ()))))

(* The pg_dump a squash runs must be the server's own version, which only
   the suite's runner can name: [make test] names the container's. *)
let pg_dump = Sys.getenv_opt "ROWTYPE_TEST_PG_DUMP"

(* A dump is what the migrations make from nothing, the same on every run,
   with the versions of the server and of pg_dump left out, and never what
   the database the URL names holds. *)
let test_a_dump_is_what_the_migrations_make () =
  match pg_dump with
  | None ->
      print_endline
        "  [dump skipped: no ROWTYPE_TEST_PG_DUMP -- make test names one]";
      Alcotest.skip ()
  | Some pg_dump ->
      let migrations =
        [
          {
            Rowtype_migrate.version = 20260101000000;
            name = "a";
            kind = Transaction;
            sql = "create table made (n int)";
          };
        ]
      in
      Db_target.with_postgres (fun target ->
          let db = Db_target.connect target in
          ok (Pg.exec_raw db "create table drifted (n int)");
          Pg.close db;
          let dump () =
            ok_s
              (Rowtype_migrate.dump ~sw:(Db_target.sw ())
                 ~net:(Db_target.net ()) ~mono_clock:(Db_target.mono ())
                 ~pg_dump ~restrict_key:"rowtype" ~migrations target)
          in
          let first = dump () in
          Alcotest.(check string) "the same on every run" first (dump ());
          Alcotest.(check (list bool))
            "the migration's table, the record's, no drift, no versions"
            [ true; true; false; false ]
            (List.map (contains first)
               [
                 "CREATE TABLE public.made";
                 "CREATE TABLE public.schema_migrations";
                 "drifted";
                 "-- Dumped";
               ]))

(* A squash's baseline stands for the history it replaced: a database made
   from it and what came after holds the schema and the rows the whole
   history makes, and a database migrated before the squash takes the new
   list as its own. *)
let test_a_squash_stands_for_its_history () =
  match pg_dump with
  | None ->
      print_endline
        "  [squash skipped: no ROWTYPE_TEST_PG_DUMP -- make test names one]";
      Alcotest.skip ()
  | Some pg_dump ->
      let m version name sql =
        { Rowtype_migrate.version; name; kind = Transaction; sql }
      in
      let history =
        [
          m 20260101000000 "a" "create table a (n int)";
          m 20260102000000 "rows" "insert into a values (1), (2)";
          m 20260103000000 "later" "alter table a add column note text";
        ]
      in
      Db_target.with_postgres (fun target ->
          let baseline =
            match
              Rowtype_migrate.squash ~sw:(Db_target.sw ())
                ~net:(Db_target.net ()) ~mono_clock:(Db_target.mono ()) ~pg_dump
                ~restrict_key:"rowtype" ~through:20260102000000
                ~migrations:history target
            with
            | Ok baseline -> baseline
            | Error m -> Alcotest.failf "the squash was refused: %s" m
          in
          Alcotest.(check int)
            "its version is the last it replaced" 20260102000000
            baseline.version;
          let squashed =
            baseline
            :: List.filter
                 (fun (m : Rowtype_migrate.migration) ->
                   m.version > 20260102000000)
                 history
          in
          let rows db =
            ok
              (Pg.run db
                 (S.list ~params:S.unit
                    ~row:S.(t2 int (opt text))
                    "select n, note from a order by n")
                 ())
          in
          (* A database made from nothing, from the squashed list. *)
          let fresh = connect target in
          Fun.protect
            ~finally:(fun () -> Pg.close fresh)
            (fun () ->
              (match Rowtype_migrate.run fresh squashed with
              | Ok () -> ()
              | Error m -> Alcotest.failf "the squashed list was refused: %s" m);
              Alcotest.(check (list (pair int (option string))))
                "the rows the history wrote, and its last column"
                [ (1, None); (2, None) ]
                (rows fresh)));
      (* A database migrated before the squash takes the new list. *)
      Db_target.with_postgres (fun target ->
          let before = connect target in
          Fun.protect
            ~finally:(fun () -> Pg.close before)
            (fun () ->
              (match Rowtype_migrate.run before history with
              | Ok () -> ()
              | Error m -> Alcotest.failf "the history was refused: %s" m);
              let baseline =
                match
                  Rowtype_migrate.squash ~sw:(Db_target.sw ())
                    ~net:(Db_target.net ()) ~mono_clock:(Db_target.mono ())
                    ~pg_dump ~restrict_key:"rowtype" ~through:20260102000000
                    ~migrations:history target
                with
                | Ok b -> b
                | Error m -> Alcotest.failf "the squash was refused: %s" m
              in
              let squashed =
                baseline
                :: List.filter
                     (fun (m : Rowtype_migrate.migration) ->
                       m.version > 20260102000000)
                     history
              in
              match Rowtype_migrate.run before squashed with
              | Ok () -> ()
              | Error m ->
                  Alcotest.failf "a database from before was refused: %s" m))

let () =
  Db_target.required ~suite:"rowtype-migrate";
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Db_target.eio env ~sw;
  Alcotest.run ~and_exit:false "rowtype-migrate"
    [
      ( "the runner",
        [
          Alcotest.test_case "a migration is unbounded" `Quick
            test_a_migration_is_unbounded;
          Alcotest.test_case "a status says what is pending, and writes nothing"
            `Quick test_a_status_says_what_is_pending_and_writes_nothing;
          Alcotest.test_case "a failed migration stops where it failed" `Quick
            test_a_failed_migration_stops_where_it_failed;
          Alcotest.test_case "a lost connection names its migration" `Quick
            test_a_lost_connection_names_its_migration;
          Alcotest.test_case "a migration is held to what ran" `Quick
            test_a_migration_is_held_to_what_ran;
          Alcotest.test_case "a migration can run outside a transaction" `Quick
            test_a_migration_can_run_outside_a_transaction;
          Alcotest.test_case "a record can have a table of its own" `Quick
            test_a_record_can_have_a_table_of_its_own;
          Alcotest.test_case "a baseline stands for what it replaced" `Quick
            test_a_baseline_stands_for_what_it_replaced;
          Alcotest.test_case "two migrators take turns" `Quick
            test_two_migrators_take_turns;
          Alcotest.test_case "a dump is what the migrations make" `Quick
            test_a_dump_is_what_the_migrations_make;
          Alcotest.test_case "a squash stands for its history" `Quick
            test_a_squash_stands_for_its_history;
        ] );
      ( "the database",
        [
          Alcotest.test_case "a database is made and dropped" `Quick
            test_a_database_is_made_and_dropped;
        ] );
    ]

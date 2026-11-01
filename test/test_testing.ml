(* rowtype-test against the real Podman: a run's own server, a database of
   a case's own in it, a server handed by its URL, and the containers a
   killed run left removed by the next. *)

module Pg = Rowtype_postgres
module T = Rowtype_test

let podman env args =
  let out = Buffer.create 256 in
  match
    Eio.Process.run
      (Eio.Stdenv.process_mgr env)
      ~stdout:(Eio.Flow.buffer_sink out)
      ~stderr:(Eio.Flow.buffer_sink (Buffer.create 64))
      ("podman" :: args)
  with
  | () -> Ok (String.trim (Buffer.contents out))
  | exception Eio.Io _ -> Error ()

let exists env name = Result.is_ok (podman env [ "container"; "exists"; name ])
let ok = function Ok v -> v | Error e -> Alcotest.fail (T.error_to_string e)

let fine = function
  | Ok v -> v
  | Error e -> Alcotest.fail (Rowtype.error_to_string e)

let exec ~sw env conninfo sql =
  let db =
    fine
      (Pg.connect ~sw ~net:(Eio.Stdenv.net env)
         ~mono_clock:(Eio.Stdenv.mono_clock env)
         conninfo)
  in
  Fun.protect
    ~finally:(fun () -> Pg.close db)
    (fun () -> fine (Pg.exec_raw db sql))

let test_a_run_owns_its_server () =
  Eio_main.run @@ fun env ->
  let name =
    Eio.Switch.run @@ fun sw ->
    let server = ok (T.server ~sw env) in
    let name =
      match T.container server with
      | Some n -> n
      | None -> Alcotest.fail "no container was started"
    in
    Alcotest.(check bool) "its container runs" true (exists env name);
    let template = ok (T.template server) in
    fine
      (T.database template (fun conninfo ->
           exec ~sw env conninfo
             "create table t (n int); insert into t values (1)"));
    name
  in
  Alcotest.(check bool)
    "and is gone once the switch ends" false (exists env name)

let test_a_case_s_database_is_dropped () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let server = ok (T.server ~sw env) in
  let template = ok (T.template server) in
  let named = ref "" in
  (match
     T.database template (fun conninfo ->
         named := Pg.Conninfo.to_url conninfo;
         failwith "a case that raises")
   with
  | exception Failure _ -> ()
  | Ok () | Error _ -> Alcotest.fail "the case's failure was lost");
  let db =
    fine
      (Pg.connect ~sw ~net:(Eio.Stdenv.net env)
         ~mono_clock:(Eio.Stdenv.mono_clock env)
         (T.conninfo server))
  in
  let count =
    fine
      (Pg.run db
         (Rowtype.find ~params:Rowtype.unit ~row:Rowtype.int
            "select count(*)::int from pg_database where datname like \
             'rowtype_test_%'")
         ())
  in
  Pg.close db;
  Alcotest.(check int) "no database left, though the case raised" 0 count

let test_a_handed_server_starts_nothing () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let own = ok (T.server ~sw env) in
  let before =
    podman env [ "ps"; "-aq"; "--filter"; "label=rowtype.test.pid" ]
  in
  let handed =
    ok (T.server ~sw ~url:(Pg.Conninfo.to_url (T.conninfo own)) env)
  in
  Alcotest.(check (option string))
    "no container of its own" None (T.container handed);
  Alcotest.(check bool)
    "no container started" true
    (before = podman env [ "ps"; "-aq"; "--filter"; "label=rowtype.test.pid" ]);
  let template = ok (T.template handed) in
  fine (T.database template (fun conninfo -> exec ~sw env conninfo "select 1"))

let count ~sw env conninfo sql =
  let db =
    fine
      (Pg.connect ~sw ~net:(Eio.Stdenv.net env)
         ~mono_clock:(Eio.Stdenv.mono_clock env)
         conninfo)
  in
  Fun.protect
    ~finally:(fun () -> Pg.close db)
    (fun () ->
      fine
        (Pg.run db (Rowtype.find ~params:Rowtype.unit ~row:Rowtype.int sql) ()))

(* What a suite's setup made once -- tables, and rows its migrations seed --
   is in every case's copy, and a case's own rows in no other's. *)
let test_a_copy_is_the_setup_s () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let server = ok (T.server ~sw env) in
  let template =
    ok
      (T.template server ~setup:(fun conninfo ->
           exec ~sw env conninfo
             "create table plan (name text); insert into plan values ('free')";
           Ok ()))
  in
  let rows () =
    fine
      (T.database template (fun conninfo ->
           let seeded =
             count ~sw env conninfo "select count(*)::int from plan"
           in
           exec ~sw env conninfo "insert into plan values ('mine')";
           seeded))
  in
  Alcotest.(check int) "the seeded row, in the first copy" 1 (rows ());
  Alcotest.(check int) "and only it in the next" 1 (rows ())

(* A template goes with the run: on a server handed, nothing of it stays. *)
let test_a_template_goes_with_the_run () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let server = ok (T.server ~sw env) in
  let url = Pg.Conninfo.to_url (T.conninfo server) in
  Eio.Switch.run (fun inner ->
      let handed = ok (T.server ~sw:inner ~url env) in
      ignore (ok (T.template handed) : T.template));
  Alcotest.(check int)
    "no template left" 0
    (count ~sw env (T.conninfo server)
       "select count(*)::int from pg_database where datname like \
        'rowtype_template_%'")

(* A process that has ended, by its id: a run killed while its container
   ran. *)
let ended_pid env =
  Eio.Switch.run @@ fun sw ->
  let p = Eio.Process.spawn ~sw (Eio.Stdenv.process_mgr env) [ "true" ] in
  ignore (Eio.Process.await p : Eio.Process.exit_status);
  Eio.Process.pid p

let test_a_killed_run_s_container_is_removed () =
  Eio_main.run @@ fun env ->
  let left = Printf.sprintf "rowtype-test-left-%d" (Unix.getpid ()) in
  (match
     podman env
       [
         "run";
         "-d";
         "--name";
         left;
         "--label";
         Printf.sprintf "rowtype.test.pid=%d" (ended_pid env);
         "docker.io/library/postgres:18-alpine";
         "sleep";
         "600";
       ]
   with
  | Ok _ -> ()
  | Error () -> Alcotest.fail "could not leave a container behind");
  Eio.Switch.run (fun sw -> ignore (ok (T.server ~sw env) : T.server));
  Alcotest.(check bool) "the next run removed it" false (exists env left)

let () =
  Alcotest.run "rowtype-test"
    [
      ( "server",
        [
          Alcotest.test_case "a run owns its server" `Slow
            test_a_run_owns_its_server;
          Alcotest.test_case "a case's database is dropped" `Slow
            test_a_case_s_database_is_dropped;
          Alcotest.test_case "a handed server starts nothing" `Slow
            test_a_handed_server_starts_nothing;
          Alcotest.test_case "a copy is the setup's" `Slow
            test_a_copy_is_the_setup_s;
          Alcotest.test_case "a template goes with the run" `Slow
            test_a_template_goes_with_the_run;
          Alcotest.test_case "a killed run's container is removed" `Slow
            test_a_killed_run_s_container_is_removed;
        ] );
    ]

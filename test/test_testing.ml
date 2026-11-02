(* rowtype-test against a real container runtime: a run's own server, a
   database of a case's own in it, a server handed by its URL, and the
   containers a killed run left removed by the next. *)

module Pg = Rowtype_postgres
module T = Rowtype_test

(* The runtime the cases start their containers in, Podman unless
   ROWTYPE_TEST_RUNTIME names Docker, as CI's Docker job does. *)
let runtime =
  match Sys.getenv_opt "ROWTYPE_TEST_RUNTIME" with
  | None | Some "podman" -> `Podman
  | Some "docker" -> `Docker
  | Some other ->
      prerr_endline ("ROWTYPE_TEST_RUNTIME is podman or docker, not " ^ other);
      exit 2

let binary = match runtime with `Podman -> "podman" | `Docker -> "docker"

let is_ours = function
  | `Podman -> ( match runtime with `Podman -> true | `Docker -> false)
  | `Docker -> ( match runtime with `Docker -> true | `Podman -> false)

let cli env args =
  let out = Buffer.create 256 in
  match
    Eio.Process.run
      (Eio.Stdenv.process_mgr env)
      ~stdout:(Eio.Flow.buffer_sink out)
      ~stderr:(Eio.Flow.buffer_sink (Buffer.create 64))
      (binary :: args)
  with
  | () -> Ok (String.trim (Buffer.contents out))
  | exception Eio.Io _ -> Error ()

let exists env name =
  Result.is_ok (cli env [ "container"; "inspect"; "--format"; "{{.Id}}"; name ])

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
    let server = ok (T.server ~sw ~runtime env) in
    let name =
      match T.container server with
      | Some (r, n) when is_ours r -> n
      | Some _ -> Alcotest.fail "started in another runtime"
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
  let server = ok (T.server ~sw ~runtime env) in
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
  let own = ok (T.server ~sw ~runtime env) in
  let before = cli env [ "ps"; "-aq"; "--filter"; "label=rowtype.test.pid" ] in
  let handed =
    ok (T.server ~sw ~runtime ~url:(Pg.Conninfo.to_url (T.conninfo own)) env)
  in
  Alcotest.(check (option string))
    "no container of its own" None
    (Option.map snd (T.container handed));
  Alcotest.(check bool)
    "no container started" true
    (before = cli env [ "ps"; "-aq"; "--filter"; "label=rowtype.test.pid" ]);
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
  let server = ok (T.server ~sw ~runtime env) in
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

(* A case cancelled inside its database -- a time limit, a sibling that
   failed -- leaves no database, and its cancellation passes as itself. *)
let test_a_cancelled_case_s_database_is_dropped () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let server = ok (T.server ~sw ~runtime env) in
  let template = ok (T.template server) in
  let entered, enter = Eio.Promise.create () in
  (match
     Eio.Fiber.first
       (fun () ->
         match
           T.database template (fun _ ->
               Eio.Promise.resolve enter ();
               Eio.Fiber.await_cancel ())
         with
         | Ok () | Error _ -> `Case)
       (fun () ->
         Eio.Promise.await entered;
         `Limit)
   with
  | `Limit -> ()
  | `Case -> Alcotest.fail "the case outlived its cancellation");
  Alcotest.(check int)
    "no database left, though the case was cancelled" 0
    (count ~sw env (T.conninfo server)
       "select count(*)::int from pg_database where datname like \
        'rowtype_test_%'")

exception Raised

(* A case that makes its own copy a template, which DROP DATABASE refuses:
   a copy left behind, and the name it was left under. *)
let undroppable ~sw env conninfo =
  let db =
    fine
      (Pg.connect ~sw ~net:(Eio.Stdenv.net env)
         ~mono_clock:(Eio.Stdenv.mono_clock env)
         conninfo)
  in
  Fun.protect
    ~finally:(fun () -> Pg.close db)
    (fun () ->
      let name =
        fine
          (Pg.run db
             (Rowtype.find ~params:Rowtype.unit ~row:Rowtype.text
                "select current_database()")
             ())
      in
      fine (Pg.exec_raw db ("alter database " ^ name ^ " is_template true"));
      name)

let remove ~sw env server name =
  List.iter
    (exec ~sw env (T.conninfo server))
    [ "alter database " ^ name ^ " is_template false"; "drop database " ^ name ]

(* What the run's cleanup could not remove is said: a copy left behind is
   the case's failure, where the case answered. *)
let test_a_copy_left_is_a_failure () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let server = ok (T.server ~sw ~runtime env) in
  let template = ok (T.template server) in
  let named = ref "" in
  (match
     T.database template (fun conninfo -> named := undroppable ~sw env conninfo)
   with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "a copy left behind was not said");
  remove ~sw env server !named

(* Where the case raised, its exception passes, and the copy left is a
   warning on rowtype-test, naming it. *)
let test_a_copy_left_by_a_raise_is_a_warning () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let server = ok (T.server ~sw ~runtime env) in
  let template = ok (T.template server) in
  let warned = ref [] in
  let reporter =
    {
      Logs.report =
        (fun src level ~over k msgf ->
          msgf (fun ?header:_ ?tags:_ fmt ->
              Format.kasprintf
                (fun line ->
                  (match level with
                  | Logs.Warning
                    when String.equal (Logs.Src.name src) "rowtype-test" ->
                      warned := line :: !warned
                  | _ -> ());
                  over ();
                  k ())
                fmt));
    }
  in
  let before = Logs.reporter () in
  Logs.set_reporter reporter;
  let named = ref "" in
  Fun.protect
    ~finally:(fun () -> Logs.set_reporter before)
    (fun () ->
      match
        T.database template (fun conninfo ->
            named := undroppable ~sw env conninfo;
            raise Raised)
      with
      | exception Raised -> ()
      | Ok () | Error _ -> Alcotest.fail "the case's raise was lost");
  remove ~sw env server !named;
  Alcotest.(check bool)
    "a warning names the copy" true
    (List.exists (String.includes ~affix:!named) !warned)

(* A setup's failure is the template's, as itself: its own tags pass
   through, merged with rowtype-test's, so a migration's refusal is matched
   as the migration words it. *)
let test_a_setup_s_failure_is_its_own () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let server = ok (T.server ~sw ~runtime env) in
  match T.template server ~setup:(fun _ -> Error `Seeded_wrong) with
  | Error `Seeded_wrong -> ()
  | Error _ -> Alcotest.fail "the setup's failure came back as another"
  | Ok _ -> Alcotest.fail "a setup that failed made a template"

(* A runtime that is not there is named, with what to do: the binary not
   on PATH, rather than an empty failure. *)
let test_a_missing_runtime_is_named () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let path = Sys.getenv_opt "PATH" in
  Unix.putenv "PATH" "/nonexistent";
  let answer =
    Fun.protect
      ~finally:(fun () -> Unix.putenv "PATH" (Option.value path ~default:""))
      (fun () -> T.server ~sw ~runtime env)
  in
  match answer with
  | Error (`Runtime (r, why)) when is_ours r ->
      Alcotest.(check bool)
        ("names PATH: " ^ why) true
        (String.includes ~affix:"PATH" why)
  | Error e -> Alcotest.fail ("another failure: " ^ T.error_to_string e)
  | Ok _ -> Alcotest.fail "a server without its runtime"

(* A url that is no connection string is refused before anything starts. *)
let test_an_unreadable_url_is_refused () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  match T.server ~sw ~runtime ~url:"not a connection string" env with
  | Error (`Unreadable _) -> ()
  | Error e -> Alcotest.fail ("another failure: " ^ T.error_to_string e)
  | Ok _ -> Alcotest.fail "a server from no connection string"

(* A connection a setup or a case leaves open keeps nothing: the template
   is still copied, and the copy still dropped. *)
let test_connections_left_open_hold_nothing () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let server = ok (T.server ~sw ~runtime env) in
  let leave_open conninfo =
    ignore
      (fine
         (Pg.connect ~sw ~net:(Eio.Stdenv.net env)
            ~mono_clock:(Eio.Stdenv.mono_clock env)
            conninfo)
        : Pg.conn)
  in
  let template =
    ok
      (T.template server ~setup:(fun conninfo ->
           leave_open conninfo;
           Ok ()))
  in
  fine (T.database template leave_open);
  Alcotest.(check int)
    "no database left, though the case left a connection" 0
    (count ~sw env (T.conninfo server)
       "select count(*)::int from pg_database where datname like \
        'rowtype_test_%'")

exception Failed

(* A run whose switch fails leaves nothing: its container is removed, and
   on a server handed, its template dropped. *)
let test_a_failed_run_leaves_nothing () =
  Eio_main.run @@ fun env ->
  let name = ref "" in
  (match
     Eio.Switch.run (fun sw ->
         let server = ok (T.server ~sw ~runtime env) in
         (match T.container server with
         | Some (_, n) -> name := n
         | None -> Alcotest.fail "no container was started");
         ignore (ok (T.template server) : T.template);
         raise Failed)
   with
  | () -> Alcotest.fail "the run's failure was lost"
  | exception Failed -> ());
  Alcotest.(check bool) "its container is gone" false (exists env !name);
  Eio.Switch.run @@ fun sw ->
  let server = ok (T.server ~sw ~runtime env) in
  let url = Pg.Conninfo.to_url (T.conninfo server) in
  (match
     Eio.Switch.run (fun inner ->
         let handed = ok (T.server ~sw:inner ~runtime ~url env) in
         ignore (ok (T.template handed) : T.template);
         raise Failed)
   with
  | () -> Alcotest.fail "the run's failure was lost"
  | exception Failed -> ());
  Alcotest.(check int)
    "and on a server handed, its template" 0
    (count ~sw env (T.conninfo server)
       "select count(*)::int from pg_database where datname like \
        'rowtype_template_%'")

(* A template goes with the run: on a server handed, nothing of it stays. *)
let test_a_template_goes_with_the_run () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let server = ok (T.server ~sw ~runtime env) in
  let url = Pg.Conninfo.to_url (T.conninfo server) in
  Eio.Switch.run (fun inner ->
      let handed = ok (T.server ~sw:inner ~runtime ~url env) in
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
     cli env
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
  Eio.Switch.run (fun sw -> ignore (ok (T.server ~sw ~runtime env) : T.server));
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
          Alcotest.test_case "a cancelled case's database is dropped" `Slow
            test_a_cancelled_case_s_database_is_dropped;
          Alcotest.test_case "a copy left is a failure" `Slow
            test_a_copy_left_is_a_failure;
          Alcotest.test_case "a copy left by a raise is a warning" `Slow
            test_a_copy_left_by_a_raise_is_a_warning;
          Alcotest.test_case "a handed server starts nothing" `Slow
            test_a_handed_server_starts_nothing;
          Alcotest.test_case "a copy is the setup's" `Slow
            test_a_copy_is_the_setup_s;
          Alcotest.test_case "a setup's failure is its own" `Slow
            test_a_setup_s_failure_is_its_own;
          Alcotest.test_case "a missing runtime is named" `Quick
            test_a_missing_runtime_is_named;
          Alcotest.test_case "an unreadable url is refused" `Quick
            test_an_unreadable_url_is_refused;
          Alcotest.test_case "connections left open hold nothing" `Slow
            test_connections_left_open_hold_nothing;
          Alcotest.test_case "a failed run leaves nothing" `Slow
            test_a_failed_run_leaves_nothing;
          Alcotest.test_case "a template goes with the run" `Slow
            test_a_template_goes_with_the_run;
          Alcotest.test_case "a killed run's container is removed" `Slow
            test_a_killed_run_s_container_is_removed;
        ] );
    ]

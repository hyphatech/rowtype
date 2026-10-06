(* A pool lends its connections to more fibers than it has, each lent as
   it was first lent, and refuses a borrow that waits too long; a listener
   hears what it listens to, in order, and through a lost connection. *)

module S = Rowtype
module Pg = Rowtype_postgres

let ok = function
  | Ok v -> v
  | Error e -> Alcotest.failf "unexpected error: %s" (S.error_to_string e)

let conninfo target = ok (Pg.conninfo target)

let with_pool ?(size = 4) ?wait_s target f =
  let pool =
    ok
      (Pg.Pool.create ~sw:(Db_target.sw ()) ~net:(Db_target.net ())
         ~mono_clock:(Db_target.mono ()) ~size ?wait_s (conninfo target))
  in
  Fun.protect ~finally:(fun () -> Pg.Pool.close pool) (fun () -> f pool)

let lent pool f =
  match Pg.Pool.use pool f with
  | Ok v -> v
  | Error `Busy -> Alcotest.fail "busy"

let echo = S.find ~params:S.int ~row:S.int "select $1::int"

(* More fibers than connections, each asking its own question many times:
   every answer is the asker's, and every connection comes back. *)
let test_more_fibers_than_connections () =
  Db_target.with_postgres (fun target ->
      with_pool ~size:4 target (fun pool ->
          let fibers = 32 and each = 20 in
          let answered = Atomic.make 0 in
          Eio.Fiber.all
            (List.init fibers (fun f () ->
                 for i = 1 to each do
                   let asked = (f * 1000) + i in
                   let got = lent pool (fun db -> ok (Pg.run db echo asked)) in
                   Alcotest.(check int) "the asker's own answer" asked got;
                   Atomic.incr answered
                 done));
          Alcotest.(check int)
            "every question answered" (fibers * each) (Atomic.get answered);
          let stats = Pg.Pool.stats pool in
          Alcotest.(check int) "every connection back" 4 stats.idle;
          Alcotest.(check int) "nobody waiting" 0 stats.waiting))

(* What one borrower leaves in its session -- a setting, a temporary table
   -- the next borrower of the same connection does not see. *)
let test_a_borrower_leaves_nothing_behind () =
  Db_target.with_postgres (fun target ->
      with_pool ~size:1 target (fun pool ->
          let setting =
            S.find ~params:S.unit ~row:S.text "show application_name"
          in
          let temp =
            S.find ~params:S.unit ~row:S.bool
              "select to_regclass('pg_temp.left_behind') is not null"
          in
          let before = lent pool (fun db -> ok (Pg.run db setting ())) in
          lent pool (fun db ->
              ok (Pg.exec_raw db "set application_name = 'left behind'");
              ok (Pg.exec_raw db "create temp table left_behind (n int)"));
          lent pool (fun db ->
              Alcotest.(check string)
                "the setting is the session's own again" before
                (ok (Pg.run db setting ()));
              Alcotest.(check bool)
                "the temporary table is gone" false
                (ok (Pg.run db temp ())))))

(* A borrow that finds no connection free within its wait is [`Busy], and
   the pool lends again once one comes back. *)
let test_a_borrow_past_its_wait_is_busy () =
  Db_target.with_postgres (fun target ->
      with_pool ~size:1 target (fun pool ->
          let release, released = Eio.Promise.create () in
          Eio.Fiber.both
            (fun () -> lent pool (fun _ -> Eio.Promise.await release))
            (fun () ->
              (match Pg.Pool.use ~wait_s:0.05 pool (fun _ -> ()) with
              | Error `Busy -> ()
              | Ok () -> Alcotest.fail "lent a connection already lent");
              Eio.Promise.resolve released ());
          Alcotest.(check int)
            "and lends again once it is back" 7
            (lent pool (fun db -> ok (Pg.run db echo 7)))))

(* ------------------------------------------------------------------ *)
(* Notifications *)

let with_listener ?heartbeat_s target f =
  let l =
    ok
      (Pg.Listener.connect ~sw:(Db_target.sw ()) ~net:(Db_target.net ())
         ~mono_clock:(Db_target.mono ()) ?heartbeat_s (conninfo target))
  in
  Fun.protect ~finally:(fun () -> Pg.Listener.close l) (fun () -> f l)

let notify db channel payload =
  ignore
    (ok
       (Pg.run db
          (S.find
             ~params:S.(t2 text text)
             ~row:S.unit "select pg_notify($1, $2)")
          (channel, payload)))

let heard l =
  match ok (Pg.Listener.next l) with
  | Pg.Listener.Notification { channel; payload } -> (channel, payload)
  | Pg.Listener.Reconnected -> Alcotest.fail "reconnected, not a notification"

let notification = Alcotest.(pair string string)

(* Notifications on every channel listened to, in the order they were
   sent, and none from a channel nobody listens to. *)
let test_a_listener_hears_its_channels_in_order () =
  Db_target.with_postgres (fun target ->
      let db = Db_target.connect target in
      Fun.protect
        ~finally:(fun () -> Pg.close db)
        (fun () ->
          with_listener target (fun l ->
              ok (Pg.Listener.listen l "jobs");
              ok (Pg.Listener.listen l "Mail");
              notify db "jobs" "1";
              notify db "nobody" "lost";
              notify db "Mail" "2";
              notify db "jobs" "3";
              Alcotest.check notification "first" ("jobs", "1") (heard l);
              Alcotest.check notification "a channel named in capitals"
                ("Mail", "2") (heard l);
              Alcotest.check notification "third" ("jobs", "3") (heard l))))

(* A listener whose connection the server dropped says so, listens again,
   and hears what is sent after. *)
let test_a_listener_survives_a_lost_connection () =
  Db_target.with_postgres (fun target ->
      let db = Db_target.connect target in
      Fun.protect
        ~finally:(fun () -> Pg.close db)
        (fun () ->
          with_listener ~heartbeat_s:0.1 target (fun l ->
              ok (Pg.Listener.listen l "jobs");
              ignore
                (ok
                   (Pg.run db
                      (S.list ~params:S.unit ~row:S.bool
                         "select pg_terminate_backend(pid) from \
                          pg_stat_activity where query ilike 'listen%' and \
                          datname = current_database()")
                      ())
                  : bool list);
              (match ok (Pg.Listener.next l) with
              | Pg.Listener.Reconnected -> ()
              | Pg.Listener.Notification _ ->
                  Alcotest.fail "a notification before the reconnection");
              notify db "jobs" "after";
              Alcotest.check notification "heard after it" ("jobs", "after")
                (heard l))))

(* A channel unlistened is heard no more, and the others still are. *)
let test_a_channel_unlistened_is_not_heard () =
  Db_target.with_postgres (fun target ->
      let db = Db_target.connect target in
      Fun.protect
        ~finally:(fun () -> Pg.close db)
        (fun () ->
          with_listener target (fun l ->
              ok (Pg.Listener.listen l "jobs");
              ok (Pg.Listener.listen l "Mail");
              ok (Pg.Listener.unlisten l "jobs");
              notify db "jobs" "lost";
              notify db "Mail" "kept";
              Alcotest.check notification "only the one still listened to"
                ("Mail", "kept") (heard l))))

(* Startup parameters reach the listener's session. *)
let test_a_listener_takes_parameters () =
  Db_target.with_postgres (fun target ->
      let db = Db_target.connect target in
      Fun.protect
        ~finally:(fun () -> Pg.close db)
        (fun () ->
          let l =
            ok
              (Pg.Listener.connect ~sw:(Db_target.sw ()) ~net:(Db_target.net ())
                 ~mono_clock:(Db_target.mono ())
                 ~parameters:[ ("application_name", "a listener") ]
                 (conninfo target))
          in
          Fun.protect
            ~finally:(fun () -> Pg.Listener.close l)
            (fun () ->
              ok (Pg.Listener.listen l "jobs");
              Alcotest.(check int)
                "its session, named" 1
                (ok
                   (Pg.run db
                      (S.find ~params:S.unit ~row:S.int
                         "select count(*)::int from pg_stat_activity where \
                          application_name = 'a listener' and datname = \
                          current_database()")
                      ())))))

let test_a_closed_listener_hears_nothing () =
  Db_target.with_postgres (fun target ->
      with_listener target (fun l ->
          Pg.Listener.close l;
          match Pg.Listener.next l with
          | Error `Closed -> ()
          | Error (`Db _ | `Conflict _ | `Not_serializable _ | `Lost _) ->
              Alcotest.fail "not the closed connection's failure"
          | Ok _ -> Alcotest.fail "a closed listener heard something"))

let () =
  Db_target.required ~suite:"pool";
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Db_target.eio env ~sw;
  Alcotest.run ~and_exit:false "pool"
    [
      ( "pool",
        [
          Alcotest.test_case "more fibers than connections" `Quick
            test_more_fibers_than_connections;
          Alcotest.test_case "a borrower leaves nothing behind" `Quick
            test_a_borrower_leaves_nothing_behind;
          Alcotest.test_case "a borrow past its wait is busy" `Quick
            test_a_borrow_past_its_wait_is_busy;
        ] );
      ( "listener",
        [
          Alcotest.test_case "it hears its channels in order" `Quick
            test_a_listener_hears_its_channels_in_order;
          Alcotest.test_case "it survives a lost connection" `Quick
            test_a_listener_survives_a_lost_connection;
          Alcotest.test_case "a closed one hears nothing" `Quick
            test_a_closed_listener_hears_nothing;
          Alcotest.test_case "a channel unlistened is not heard" `Quick
            test_a_channel_unlistened_is_not_heard;
          Alcotest.test_case "it takes parameters" `Quick
            test_a_listener_takes_parameters;
        ] );
    ]

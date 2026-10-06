(* What each failure is told as, that the connection reads on after every
   one, and that no failure and no log line carries a parameter's value. *)

module S = Rowtype
module Pg = Rowtype_postgres

let ok = function
  | Ok v -> v
  | Error e -> Alcotest.failf "unexpected error: %s" (S.error_to_string e)

let on_db setup f =
  Db_target.with_postgres (fun target ->
      let db = Db_target.connect target in
      Fun.protect
        ~finally:(fun () -> Pg.close db)
        (fun () ->
          List.iter (fun s -> ok (Pg.exec_raw db s)) setup;
          f db))

let exec db sql = Pg.run db (S.exec ~params:S.unit sql) ()

(* A failure as the test reads it: which variant, and for a conflict the
   constraint. *)
type told = Conflict of string option | Not_serializable | Closed | Lost | Db

let told = function
  | Ok () -> Alcotest.fail "the statement succeeded"
  | Error (`Conflict name) -> Conflict name
  | Error (`Not_serializable _) -> Not_serializable
  | Error `Closed -> Closed
  | Error (`Lost _) -> Lost
  | Error (`Db _) -> Db

let print_told = function
  | Conflict (Some n) -> "Conflict " ^ n
  | Conflict None -> "Conflict"
  | Not_serializable -> "Not_serializable"
  | Closed -> "Closed"
  | Lost -> "Lost"
  | Db -> "Db"

let equal_told a b =
  match (a, b) with
  | Conflict x, Conflict y -> Option.equal String.equal x y
  | Not_serializable, Not_serializable | Closed, Closed | Lost, Lost | Db, Db ->
      true
  | (Conflict _ | Not_serializable | Closed | Lost | Db), _ -> false

let told_t =
  Alcotest.testable
    (fun f t -> Format.pp_print_string f (print_told t))
    equal_told

let reads_on db what =
  Alcotest.(check int)
    (what ^ ", and the connection reads on")
    1
    (ok (Pg.run db (S.find ~params:S.unit ~row:S.int "select 1") ()))

(* Every failure a statement can meet on its own, each told as what it is,
   and none of them leaving the connection unable to read on. *)
let test_each_failure_is_told_as_what_it_is () =
  on_db
    [
      "create table u (k int constraint u_k_key unique, n int not null, c int \
       constraint u_c_check check (c > 0))";
      "insert into u values (1, 1, 1)";
    ] (fun db ->
      List.iter
        (fun (what, sql, expected) ->
          Alcotest.check told_t what expected (told (exec db sql));
          reads_on db what)
        [
          ( "a unique key taken",
            "insert into u values (1, 1, 1)",
            Conflict (Some "u_k_key") );
          ( "a NULL where none is allowed",
            "insert into u values (2, null, 1)",
            Db );
          ("a value its check refuses", "insert into u values (2, 1, 0)", Db);
          ("a syntax error", "selec 1", Db);
          ("a column that is not there", "select nope from u", Db);
          ("a division by zero", "select 1 / 0", Db);
          ("a value that is not its type", "select 'x'::int", Db);
        ])

(* A statement past its timeout, and a lock not given in time, are each a
   failure the connection survives. *)
let test_timeouts_leave_the_connection_whole () =
  Db_target.with_postgres (fun target ->
      let db = Db_target.connect target and holder = Db_target.connect target in
      Fun.protect
        ~finally:(fun () ->
          Pg.close db;
          Pg.close holder)
        (fun () ->
          ok (Pg.exec_raw db "create table l (k int)");
          ok (Pg.exec_raw db "insert into l values (1)");
          ok (Pg.exec_raw db "set statement_timeout = 50");
          Alcotest.check told_t "a statement past its timeout" Db
            (told (exec db "select pg_sleep(1)"));
          reads_on db "a statement past its timeout";
          ok (Pg.exec_raw db "set statement_timeout = 0");
          ok (Pg.exec_raw holder "begin; lock table l");
          ok (Pg.exec_raw db "set lock_timeout = 50");
          Alcotest.check told_t "a lock not given in time" Db
            (told (exec db "update l set k = 2"));
          ok (Pg.exec_raw holder "rollback");
          reads_on db "a lock not given in time"))

(* Two transactions each holding the row the other wants: Postgres breaks
   the cycle by refusing one, which is told as the one failure that
   running again may get past. *)
let test_a_deadlock_is_not_serializable () =
  Db_target.with_postgres (fun target ->
      let a = Db_target.connect target and b = Db_target.connect target in
      Fun.protect
        ~finally:(fun () ->
          Pg.close a;
          Pg.close b)
        (fun () ->
          ok (Pg.exec_raw a "create table d (k int primary key, v int)");
          ok (Pg.exec_raw a "insert into d values (1, 0), (2, 0)");
          List.iter
            (fun db -> ok (Pg.exec_raw db "set deadlock_timeout = '50ms'"))
            [ a; b ];
          let update db k =
            exec db (Printf.sprintf "update d set v = v + 1 where k = %d" k)
          in
          ok (Pg.exec_raw a "begin");
          ok (Pg.exec_raw b "begin");
          ok (update a 1);
          ok (update b 2);
          let first, second =
            Eio.Fiber.pair (fun () -> update a 2) (fun () -> update b 1)
          in
          let refused =
            List.filter_map
              (function Ok () -> None | e -> Some (told e))
              [ first; second ]
          in
          Alcotest.(check (list told_t))
            "one is refused as not serializable, the other goes on"
            [ Not_serializable ] refused;
          ignore (Pg.exec_raw a "rollback");
          ignore (Pg.exec_raw b "rollback");
          reads_on a "after a deadlock";
          reads_on b "after a deadlock"))

(* ------------------------------------------------------------------ *)
(* No value in a failure or a log line *)

let secret = "s3cr3t-7f2e9c"

let contains haystack needle =
  let n = String.length needle and h = String.length haystack in
  let rec go i =
    i + n <= h && (String.equal (String.sub haystack i n) needle || go (i + 1))
  in
  go 0

(* Every line written at any level while [f] runs. *)
let logged f =
  let lines = ref [] in
  let report _src _level ~over k msgf =
    msgf (fun ?header:_ ?tags:_ fmt ->
        Format.kasprintf
          (fun s ->
            lines := s :: !lines;
            over ();
            k ())
          fmt)
  in
  let before = Logs.reporter () and level = Logs.level () in
  Logs.set_reporter { Logs.report };
  Logs.set_level (Some Logs.Debug);
  Fun.protect
    ~finally:(fun () ->
      Logs.set_reporter before;
      Logs.set_level level)
    (fun () -> f ());
  !lines

(* The secret is a parameter of every statement that fails here, in every
   way a statement can, and of ones that succeed: it reaches no failure's
   words and no line of the log, at any level. *)
let test_no_value_in_a_failure_or_a_log () =
  on_db
    [
      "create table s (k text constraint s_k_key unique, n text not null, c \
       text constraint s_c_check check (c <> 'x'))";
    ] (fun db ->
      let failures = ref [] in
      let run st args =
        match Pg.run db st args with
        | Ok _ -> ()
        | Error e -> failures := S.error_to_string e :: !failures
      in
      let insert =
        S.exec
          ~params:S.(t3 text (opt text) text)
          "insert into s values ($1, $2, $3)"
      in
      let lines =
        logged (fun () ->
            run insert (secret, Some secret, secret);
            run insert (secret, Some secret, secret);
            run insert (secret ^ "2", None, secret);
            run insert (secret ^ "3", Some secret, "x");
            run (S.find ~params:S.text ~row:S.int "select $1::int") secret;
            run (S.find ~params:S.text ~row:S.int "select $1") secret;
            run
              (S.list ~params:S.text ~row:S.text "select k from s where k = $1")
              secret)
      in
      List.iter
        (fun m ->
          if contains m secret then
            Alcotest.failf "a failure carries the value: %s" m)
        !failures;
      List.iter
        (fun l ->
          if contains l secret then
            Alcotest.failf "a log line carries the value: %s" l)
        lines;
      Alcotest.(check bool)
        "there were failures to look at" true
        (List.length !failures >= 5))

let () =
  Db_target.required ~suite:"errors";
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Db_target.eio env ~sw;
  Alcotest.run ~and_exit:false "errors"
    [
      ( "told",
        [
          Alcotest.test_case "each failure is told as what it is" `Quick
            test_each_failure_is_told_as_what_it_is;
          Alcotest.test_case "timeouts leave the connection whole" `Quick
            test_timeouts_leave_the_connection_whole;
          Alcotest.test_case "a deadlock is not serializable" `Quick
            test_a_deadlock_is_not_serializable;
        ] );
      ( "secrets",
        [
          Alcotest.test_case "no value in a failure or a log" `Quick
            test_no_value_in_a_failure_or_a_log;
        ] );
    ]

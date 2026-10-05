(* A COPY's rows, bound by a shape as a statement's parameters are: every
   kind of value comes back as itself, a load is read as it is sent, and
   every failure writes nothing and leaves the connection usable. *)

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

let count db table =
  ok
    (Pg.run db
       (S.find ~params:S.unit ~row:S.int ("select count(*) from " ^ table))
       ())

let reads_on db what =
  Alcotest.(check int)
    (what ^ ", and the connection reads on")
    1
    (ok (Pg.run db (S.find ~params:S.unit ~row:S.int "select 1") ()))

let column db ty sql = ok (Pg.run db (S.list ~params:S.unit ~row:ty sql) ())

(* Text that COPY's own format gives a meaning to -- a tab, a newline, a
   backslash, its NULL marker -- and text an array's gives one to. *)
let texts =
  [
    "plain";
    "";
    "a\ttab";
    "a\nnewline\r\n";
    "a \\ backslash";
    "\\N";
    "NULL";
    "a \"quote\"";
    "{a,brace}";
    "\xc3\xa9t\xc3\xa9";
  ]

let test_every_value_comes_back () =
  on_db
    [
      "create table v (k int8, f float8, t text, b bytea, ok bool, at \
       timestamptz, id uuid, doc json, tags text[], n int4)";
    ] (fun db ->
      let row =
        S.t10 S.int S.float S.text S.bytes S.bool S.instant S.uuid S.json
          (S.array (S.opt S.text))
          (S.opt S.int)
      in
      let rows =
        List.mapi
          (fun k t ->
            ( k,
              float_of_int k /. 3.,
              t,
              t ^ "\000\255",
              k mod 2 = 0,
              1_700_000_000_123_456 + k,
              Printf.sprintf "0190c0fe-0000-7000-8000-%012d" k,
              Printf.sprintf {|{"k": %d}|} k,
              [ Some t; None ],
              if k mod 2 = 0 then None else Some (-k) ))
          texts
      in
      let written =
        ok (Pg.copy_in db ~table:"v" ~columns:[] row (List.to_seq rows))
      in
      Alcotest.(check int) "every row written" (List.length texts) written;
      let read ty name =
        column db ty ("select " ^ name ^ " from v order by k")
      in
      Alcotest.(check (list int))
        "int8"
        (List.mapi (fun k _ -> k) texts)
        (read S.int "k");
      Alcotest.(check (list (float 0.)))
        "float8"
        (List.mapi (fun k _ -> float_of_int k /. 3.) texts)
        (read S.float "f");
      Alcotest.(check (list string)) "text" texts (read S.text "t");
      Alcotest.(check (list string))
        "bytea"
        (List.map (fun t -> t ^ "\000\255") texts)
        (read S.bytes "b");
      Alcotest.(check (list bool))
        "bool"
        (List.mapi (fun k _ -> k mod 2 = 0) texts)
        (read S.bool "ok");
      Alcotest.(check (list int))
        "timestamptz"
        (List.mapi (fun k _ -> 1_700_000_000_123_456 + k) texts)
        (read S.instant "at");
      Alcotest.(check (list string))
        "uuid"
        (List.mapi
           (fun k _ -> Printf.sprintf "0190c0fe-0000-7000-8000-%012d" k)
           texts)
        (read S.uuid "id");
      Alcotest.(check (list string))
        "json"
        (List.mapi (fun k _ -> Printf.sprintf {|{"k": %d}|} k) texts)
        (read S.json "doc");
      Alcotest.(check (list (list (option string))))
        "text[]"
        (List.map (fun t -> [ Some t; None ]) texts)
        (read (S.array (S.opt S.text)) "tags");
      Alcotest.(check (list (option int)))
        "a NULL"
        (List.mapi (fun k _ -> if k mod 2 = 0 then None else Some (-k)) texts)
        (read (S.opt S.int) "n"))

(* Columns named in the row's order, and names quoted as given. *)
let test_columns_and_names_are_as_given () =
  on_db
    [
      "create schema \"Work\"";
      "create table \"Work\".\"Jobs\" (id int generated always as identity, \
       \"Name\" text, due int)";
    ] (fun db ->
      Alcotest.(check int)
        "written" 2
        (ok
           (Pg.copy_in db ~schema:"Work" ~table:"Jobs"
              ~columns:[ "due"; "Name" ] (S.t2 S.int S.text)
              (List.to_seq [ (2, "b"); (1, "a") ])));
      Alcotest.(check (list (pair string int)))
        "each value in its column"
        [ ("a", 1); ("b", 2) ]
        (column db (S.t2 S.text S.int)
           "select \"Name\", due from \"Work\".\"Jobs\" order by due"))

(* The sequence is read as the rows are sent: a million rows, made as they
   are asked for, never held at once. *)
let test_a_load_is_held_a_row_at_a_time () =
  on_db [ "create table big (k int8, t text)" ] (fun db ->
      let n = 1_000_000 in
      let rows = Seq.init n (fun k -> (k, Printf.sprintf "row %d" k)) in
      Gc.compact ();
      let before = (Gc.quick_stat ()).top_heap_words in
      let written =
        ok
          (Pg.copy_in db ~table:"big" ~columns:[ "k"; "t" ] (S.t2 S.int S.text)
             rows)
      in
      let grown = (Gc.quick_stat ()).top_heap_words - before in
      Alcotest.(check int) "every row written" n written;
      Alcotest.(check int) "and counted" n (count db "big");
      (* A million rows held would be tens of millions of words. *)
      if grown > 2_000_000 then
        Alcotest.failf "the heap grew by %d words over the load" grown)

let test_a_constraint_writes_nothing () =
  on_db [ "create table u (k int constraint u_k_key unique)" ] (fun db ->
      (match
         Pg.copy_in db ~table:"u" ~columns:[ "k" ] S.int
           (List.to_seq [ 1; 2; 3; 1 ])
       with
      | Error (`Conflict (Some "u_k_key")) -> ()
      | Error e -> Alcotest.failf "told as: %s" (S.error_to_string e)
      | Ok n -> Alcotest.failf "%d rows written" n);
      Alcotest.(check int) "nothing written" 0 (count db "u");
      reads_on db "a conflict")

let test_a_row_that_cannot_be_bound_writes_nothing () =
  on_db [ "create table a (k int, xs int[])" ] (fun db ->
      let row = S.t2 S.int (S.array (S.t2 S.int S.int)) in
      (match
         Pg.copy_in db ~table:"a" ~columns:[ "k"; "xs" ] row
           (List.to_seq [ (1, []); (2, [ (1, 2) ]) ])
       with
      | Error (`Db _) -> ()
      | Error e -> Alcotest.failf "told as: %s" (S.error_to_string e)
      | Ok n -> Alcotest.failf "%d rows written" n);
      Alcotest.(check int) "nothing written" 0 (count db "a");
      reads_on db "an unbindable row")

exception Source_failed

let test_a_raise_from_the_rows_writes_nothing () =
  on_db [ "create table r (k int)" ] (fun db ->
      let rows =
        Seq.init 10 (fun k -> if k = 5 then raise Source_failed else k)
      in
      (match Pg.copy_in db ~table:"r" ~columns:[ "k" ] S.int rows with
      | exception Source_failed -> ()
      | Ok n -> Alcotest.failf "%d rows written" n
      | Error e -> Alcotest.failf "told as: %s" (S.error_to_string e));
      Alcotest.(check int) "nothing written" 0 (count db "r");
      reads_on db "a raise")

let test_columns_that_are_not_the_row_are_refused () =
  on_db [ "create table c (a int, b int)" ] (fun db ->
      let sent = ref false in
      let rows =
        Seq.map
          (fun v ->
            sent := true;
            v)
          (List.to_seq [ (1, 2, 3) ])
      in
      (match
         Pg.copy_in db ~table:"c" ~columns:[ "a"; "b" ] (S.t3 S.int S.int S.int)
           rows
       with
      | Error (`Db _) -> ()
      | Error e -> Alcotest.failf "told as: %s" (S.error_to_string e)
      | Ok n -> Alcotest.failf "%d rows written" n);
      Alcotest.(check bool) "nothing read from the rows" false !sent;
      reads_on db "a refusal")

let test_a_load_rolls_back_with_its_transaction () =
  on_db [ "create table t (k int)" ] (fun db ->
      (match
         Pg.Transaction.within db (fun db ->
             Result.bind
               (Pg.copy_in db ~table:"t" ~columns:[ "k" ] S.int
                  (List.to_seq [ 1; 2; 3 ]))
               (fun _ -> Error `Refused))
       with
      | Error `Refused -> ()
      | Error _ | Ok () -> Alcotest.fail "not the work's refusal");
      Alcotest.(check int) "nothing kept" 0 (count db "t"))

(* A value the database refuses is worded from its code, as a statement's
   is: COPY's own words quote the line. *)
let test_no_value_in_a_failure () =
  on_db [ "create table s (id uuid)" ] (fun db ->
      let secret = "secret-7f3a91" in
      match
        Pg.copy_in db ~table:"s" ~columns:[ "id" ] S.uuid
          (List.to_seq [ secret ])
      with
      | Ok _ -> Alcotest.fail "a value that is no uuid was written"
      | Error e ->
          let m = S.error_to_string e in
          let n = String.length secret in
          let rec carries i =
            i + n <= String.length m
            && (String.equal (String.sub m i n) secret || carries (i + 1))
          in
          if carries 0 then Alcotest.failf "the failure carries the value: %s" m;
          reads_on db "a refused value")

let () =
  Db_target.required ~suite:"copy";
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Db_target.eio env ~sw;
  Alcotest.run ~and_exit:false "copy"
    [
      ( "rows",
        [
          Alcotest.test_case "every value comes back as itself" `Quick
            test_every_value_comes_back;
          Alcotest.test_case "columns and names are as given" `Quick
            test_columns_and_names_are_as_given;
          Alcotest.test_case "a load is held a row at a time" `Quick
            test_a_load_is_held_a_row_at_a_time;
        ] );
      ( "failures",
        [
          Alcotest.test_case "a constraint writes nothing" `Quick
            test_a_constraint_writes_nothing;
          Alcotest.test_case "a row that cannot be bound writes nothing" `Quick
            test_a_row_that_cannot_be_bound_writes_nothing;
          Alcotest.test_case "a raise from the rows writes nothing" `Quick
            test_a_raise_from_the_rows_writes_nothing;
          Alcotest.test_case "columns that are not the row are refused" `Quick
            test_columns_that_are_not_the_row_are_refused;
          Alcotest.test_case "a load rolls back with its transaction" `Quick
            test_a_load_rolls_back_with_its_transaction;
          Alcotest.test_case "no value in a failure" `Quick
            test_no_value_in_a_failure;
        ] );
    ]

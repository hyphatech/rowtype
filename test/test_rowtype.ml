module S = Rowtype
module Pg = Rowtype_postgres

let ok = function
  | Ok v -> v
  | Error e -> Alcotest.failf "unexpected error: %s" (S.error_to_string e)

let contains haystack needle =
  let n = String.length needle and h = String.length haystack in
  let rec go i =
    i + n <= h && (String.equal (String.sub haystack i n) needle || go (i + 1))
  in
  go 0

(* Each case gets a database of its own. See [db_target.ml]. *)
let on_db setup f =
  Db_target.with_postgres (fun target ->
      let db = Db_target.connect target in
      Fun.protect
        ~finally:(fun () -> Pg.close db)
        (fun () ->
          List.iter (fun s -> ok (Pg.exec_raw db s)) setup;
          f db))

(* One shape drives both binding and decoding, so a roundtrip is the test
   that matters: anything that disagrees between the two directions fails
   here. *)
let test_roundtrip_t4 () =
  on_db [ "create table t (a integer, b double precision, c text, d integer)" ]
    (fun db ->
      let shape = S.(t4 int float text (opt int)) in
      ok
        (Pg.run db
           (S.exec ~params:shape "insert into t values ($1, $2, $3, $4)")
           (42, 1.5, "hi", Some 7));
      match
        ok
          (Pg.run db
             (S.list ~params:S.unit ~row:shape "select a, b, c, d from t")
             ())
      with
      | [ (42, 1.5, "hi", Some 7) ] -> ()
      | rows -> Alcotest.failf "unexpected rows (%d)" (List.length rows))

let test_option_null () =
  on_db [ "create table t (a integer, b integer)" ] (fun db ->
      let shape = S.(t2 int (opt int)) in
      ok
        (Pg.run db
           (S.exec ~params:shape "insert into t values ($1, $2)")
           (1, None));
      ok
        (Pg.run db
           (S.exec ~params:shape "insert into t values ($1, $2)")
           (2, Some 9));
      match
        ok
          (Pg.run db
             (S.list ~params:S.unit ~row:shape "select a, b from t order by a")
             ())
      with
      | [ (1, None); (2, Some 9) ] -> ()
      | _ -> Alcotest.fail "NULL did not round-trip as None")

let test_find_opt () =
  on_db [ "create table t (k integer primary key, v text)" ] (fun db ->
      ok
        (Pg.run db
           (S.exec ~params:S.(t2 int text) "insert into t values ($1, $2)")
           (1, "one"));
      Alcotest.(check (option string))
        "present" (Some "one")
        (ok
           (Pg.run db
              (S.find_opt ~params:S.int ~row:S.text
                 "select v from t where k = $1")
              1));
      Alcotest.(check (option string))
        "absent" None
        (ok
           (Pg.run db
              (S.find_opt ~params:S.int ~row:S.text
                 "select v from t where k = $1")
              2));
      ok (Pg.exec_raw db "insert into t values (2, 'two')");
      match
        Pg.run db (S.find_opt ~params:S.unit ~row:S.text "select v from t") ()
      with
      | Error (`Db m | `Not_serializable m) ->
          Alcotest.(check bool)
            ("several, said: " ^ m) true (contains m "2 rows")
      | Error (`Conflict _) -> Alcotest.fail "not a conflict"
      | Ok _ -> Alcotest.fail "several rows answered as one")

(* A uniqueness violation must be distinguishable from a real failure --
   that is what lets the HTTP layer answer 409 rather than 500, and what
   settles two writers racing for the same move. *)
let test_conflict_is_distinct () =
  on_db [ "create table u (k integer primary key)" ] (fun db ->
      ok (Pg.run db (S.exec ~params:S.int "insert into u values ($1)") 1);
      match Pg.run db (S.exec ~params:S.int "insert into u values ($1)") 1 with
      | Error (`Conflict _) -> ()
      | Error (`Db m | `Not_serializable m) ->
          Alcotest.failf "expected Conflict, got Db %S" m
      | Ok () -> Alcotest.fail "duplicate insert was accepted")

(* And through a row, because that is how an [insert ... returning id] is
   run: a generated key read back on the ROW path, where a duplicate has to
   be a conflict just as it is on the DONE one. *)
let test_conflict_through_returning () =
  on_db [ "create table u (k integer primary key)" ] (fun db ->
      let first =
        ok
          (Pg.run db
             (S.find_opt ~params:S.int ~row:S.int
                "insert into u values ($1) returning k")
             1)
      in
      Alcotest.(check (option int)) "returns the key" (Some 1) first;
      match
        Pg.run db
          (S.find_opt ~params:S.int ~row:S.int
             "insert into u values ($1) returning k")
          1
      with
      | Error (`Conflict _) -> ()
      | Error (`Db m | `Not_serializable m) ->
          Alcotest.failf "expected Conflict, got Db %S" m
      | Ok _ -> Alcotest.fail "duplicate insert was accepted")

(* Which constraint refused is the answer's meaning when a table has two:
   a duplicate address and a duplicate name are different sentences. *)
let test_a_conflict_names_its_constraint () =
  on_db
    [
      "create table users (email text constraint users_email_key unique, name \
       text constraint users_name_key unique)";
      "insert into users values ('a@b.c', 'ann')";
    ] (fun db ->
      let add =
        S.exec ~params:S.(t2 text text) "insert into users values ($1, $2)"
      in
      let refused_by args =
        match Pg.run db add args with
        | Error (`Conflict name) -> name
        | Error (`Db m | `Not_serializable m) ->
            Alcotest.failf "not a conflict: %s" m
        | Ok () -> Alcotest.fail "the duplicate was accepted"
      in
      Alcotest.(check (option string))
        "the address" (Some "users_email_key")
        (refused_by ("a@b.c", "bob"));
      Alcotest.(check (option string))
        "the name" (Some "users_name_key")
        (refused_by ("d@e.f", "ann")))

(* A constraint refuses because of other rows -- a key taken, a range
   overlapped, a parent missing or still referenced -- and that is the
   caller's answer; one about the row alone is the program's mistake. *)
let test_a_conflict_is_about_other_rows () =
  on_db
    [
      "create table parent (id int primary key)";
      "create table child (parent int constraint child_parent_fkey references \
       parent on delete restrict)";
      "create table booking (during int4range, constraint booking_overlap \
       exclude using gist (during with &&))";
      "create table person (name text not null, age int constraint \
       person_age_check check (age >= 0))";
      "insert into parent values (1)";
      "insert into child values (1)";
      "insert into booking values ('[1,5)')";
    ] (fun db ->
      let refusal sql =
        match Pg.run db (S.exec ~params:S.unit sql) () with
        | Error (`Conflict name) -> `Conflict name
        | Error (`Db _) -> `Db
        | Error (`Not_serializable m) -> Alcotest.failf "not serializable: %s" m
        | Ok () -> Alcotest.failf "accepted: %s" sql
      in
      let conflict = function `Conflict name -> name | `Db -> None in
      let is_db = function `Db -> true | `Conflict _ -> false in
      Alcotest.(check (option string))
        "a missing parent" (Some "child_parent_fkey")
        (conflict (refusal "insert into child values (2)"));
      Alcotest.(check (option string))
        "a parent still referenced" (Some "child_parent_fkey")
        (conflict (refusal "delete from parent where id = 1"));
      Alcotest.(check (option string))
        "an overlap" (Some "booking_overlap")
        (conflict (refusal "insert into booking values ('[3,8)')"));
      Alcotest.(check bool)
        "a NULL where none is allowed" true
        (is_db (refusal "insert into person values (null, 1)"));
      Alcotest.(check bool)
        "a value its check refuses" true
        (is_db (refusal "insert into person values ('ann', -1)")))

(* A fold sees what [run] would answer, in its order, without the list:
   a [list] statement's rows as they arrive, and a [find]'s one array. *)
let test_fold_is_run_without_the_list () =
  on_db [] (fun db ->
      let numbers =
        S.list ~params:S.int ~row:S.int
          "select g from generate_series(1, $1) g order by g"
      in
      let n = 10_000 in
      Alcotest.(check int)
        "every row, summed"
        (n * (n + 1) / 2)
        (ok (Pg.fold db numbers n ~init:0 ( + )));
      Alcotest.(check (list int))
        "in the order run answers"
        (ok (Pg.run db numbers 5))
        (List.rev (ok (Pg.fold db numbers 5 ~init:[] (fun acc x -> x :: acc))));
      let one_array =
        S.find ~params:S.unit ~row:(S.array S.int) "select array[3, 1, 2]"
      in
      Alcotest.(check (list int))
        "a find's array, element by element" [ 2; 1; 3 ]
        (ok (Pg.fold db one_array () ~init:[] (fun acc x -> x :: acc))))

(* A row that does not decode ends the fold as its answer: [f] sees the rows
   before it and none after, and the connection reads on. *)
let test_a_fold_stops_at_a_row_it_cannot_read () =
  on_db [] (fun db ->
      let mixed =
        S.list ~params:S.unit ~row:S.int
          "select v::int from (values ('1'), ('2'), (null), ('4')) t(v)"
      in
      let seen = ref [] in
      (match Pg.fold db mixed () ~init:() (fun () x -> seen := x :: !seen) with
      | Error (`Db _) -> ()
      | Error (`Conflict _ | `Not_serializable _) ->
          Alcotest.fail "not a decoding failure"
      | Ok () -> Alcotest.fail "a NULL read as an int");
      Alcotest.(check (list int)) "the rows before it" [ 2; 1 ] !seen;
      Alcotest.(check int)
        "and the connection reads on" 1
        (ok (Pg.run db (S.find ~params:S.unit ~row:S.int "select 1") ())))

(* [find] promises one row, and a statement that answers none or several is
   an error saying how many -- never the first of several, never a made-up
   absence. *)
let test_find_is_exactly_one_row () =
  on_db [ "create table t (n int)"; "insert into t values (1), (2)" ] (fun db ->
      let one sql = Pg.run db (S.find ~params:S.unit ~row:S.int sql) () in
      Alcotest.(check int)
        "one row" 3
        (ok (one "insert into t values (3) returning n"));
      let refused sql =
        match one sql with
        | Error (`Db m | `Not_serializable m) -> m
        | Error (`Conflict _) -> Alcotest.fail "not a conflict"
        | Ok n -> Alcotest.failf "answered %d" n
      in
      Alcotest.(check bool)
        "none, said" true
        (contains (refused "select n from t where n > 10") "0 rows");
      Alcotest.(check bool)
        "several, said" true
        (contains (refused "select n from t where n < 3") "2 rows"))

let test_type_mismatch_reported () =
  on_db [ "create table t (a text)"; "insert into t values ('x')" ] (fun db ->
      match
        Pg.run db (S.list ~params:S.unit ~row:S.int "select a from t") ()
      with
      | Error (`Db m | `Not_serializable m) ->
          Alcotest.(check bool)
            "names what was expected" true (contains m "INT");
          Alcotest.(check bool) "names the column" true (contains m "column 0")
      | Error (`Conflict _) ->
          Alcotest.fail "expected a decode error, got Conflict"
      | Ok _ -> Alcotest.fail "decoded TEXT as INT")

(* A whole number in a float column arrives as "7", which must decode. *)
let test_float_accepts_int () =
  on_db [ "create table f (x double precision)"; "insert into f values (7)" ]
    (fun db ->
      Alcotest.(check (option (float 0.001)))
        "7 decodes as 7.0" (Some 7.0)
        (ok
           (Pg.run db
              (S.find_opt ~params:S.unit ~row:S.float "select x from f")
              ())))

(* A shape's values bind [$1], [$2], ... in the order the shape gives them.
   Getting that order wrong is silent -- the query stays valid SQL about the
   wrong columns -- so it is pinned by the values landing where they were
   sent. *)
let test_placeholder_order () =
  on_db [ "create table q (a integer, b integer, c text)" ] (fun db ->
      ok
        (Pg.run db
           (S.exec
              ~params:S.(t3 int int text)
              "insert into q (a, b, c) values ($1, $2, $3)")
           (1, 2, "three"));
      Alcotest.(check (option (triple int int string)))
        "in the order they were sent"
        (Some (1, 2, "three"))
        (ok
           (Pg.run db
              (S.find_opt ~params:S.unit
                 ~row:S.(t3 int int text)
                 "select a, b, c from q")
              ())))

(* A statement says [$1] as often as it asks about the value -- the
   library listing asks three columns about one account id -- and one value
   is bound for it. *)
let test_a_parameter_used_twice () =
  on_db [ "create table owned (a integer, b integer, c text)" ] (fun db ->
      ok
        (Pg.run db
           (S.exec
              ~params:S.(t3 int int text)
              "insert into owned (a, b, c) values ($1, $2, $3)")
           (5, 6, "seven"));
      Alcotest.(check (option string))
        "$1 twice, one value bound" (Some "seven")
        (ok
           (Pg.run db
              (S.find_opt ~params:S.int ~row:S.text
                 "select c from owned where a = $1 or b = $1")
              5)))

(* A statement reaches the database as written, so Postgres's own [?]
   operator is one a statement can use. *)
let test_a_question_mark_is_postgres_s () =
  on_db
    [
      "create table tagged (n integer, tags jsonb)";
      {|insert into tagged values (1, '{"go": 1}'), (2, '{"chess": 1}')|};
    ] (fun db ->
      Alcotest.(check (list int))
        "where tags ? $1" [ 1 ]
        (ok
           (Pg.run db
              (S.list ~params:S.text ~row:S.int
                 "select n from tagged where tags ? $1 order by n")
              "go")))

(* [exec_count] is what makes a guarded update an answer: "did I get it",
   decided by the database in one statement. *)
let test_exec_count_counts_rows () =
  on_db
    [
      "create table g (k integer primary key, owner text)";
      "insert into g (k, owner) values (1, null)";
      "insert into g (k, owner) values (2, 'taken')";
    ] (fun db ->
      let claim =
        S.exec_count ~params:S.int
          "update g set owner = 'mine' where k = $1 and owner is null"
      in
      Alcotest.(check int) "claimed" 1 (ok (Pg.run db claim 1));
      Alcotest.(check int) "already taken" 0 (ok (Pg.run db claim 2)))

(* A value the database holds and this program cannot read -- an enum
   label nobody taught it -- is a decode error naming the column, never a
   raise and never a guess. *)
let test_parse_refuses_what_it_cannot_read () =
  on_db
    [
      "create table e (c text, b boolean)"; "insert into e values ('red', true)";
    ] (fun db ->
      let colour =
        S.parse S.text
          ~of_:(function "red" -> Some `Red | _ -> None)
          ~to_:(fun `Red -> "red")
      in
      Alcotest.(check bool)
        "red reads, and so does true" true
        (match
           Pg.run db
             (S.find_opt ~params:S.unit
                ~row:S.(t2 colour bool)
                "select c, b from e")
             ()
         with
        | Ok (Some (`Red, true)) -> true
        | _ -> false);
      ok (Pg.exec_raw db "insert into e values ('blue', false)");
      match
        Pg.run db
          (S.list ~params:S.unit ~row:colour "select c from e order by c")
          ()
      with
      | Error (`Db m | `Not_serializable m) ->
          Alcotest.(check bool) "names the column" true (contains m "column 0")
      | Error (`Conflict _) -> Alcotest.fail "expected a decode error"
      | Ok _ -> Alcotest.fail "decoded a value nothing reads")

(* Bytes go out and come back as themselves, every byte value among them --
   a NUL, a backslash, a quote, one past ASCII -- and the database holds
   the bytes that were sent, not their spelling. *)
let test_bytes_round_trip () =
  on_db [ "create table b (x bytea)" ] (fun db ->
      let every = String.init 256 Char.chr in
      ok (Pg.run db (S.exec ~params:S.bytes "insert into b values ($1)") every);
      Alcotest.(check (option string))
        "read back" (Some every)
        (ok
           (Pg.run db
              (S.find_opt ~params:S.unit ~row:S.bytes "select x from b")
              ()));
      Alcotest.(check (option bool))
        "and the database agrees about which bytes" (Some true)
        (ok
           (Pg.run db
              (S.find_opt ~params:S.bytes ~row:S.bool
                 "select $1 = decode('005c27ff', 'hex')")
              "\x00\\'\xff")))

(* A statement's declared shapes against what the database says of it,
   without running it: what agrees passes -- an enum read as text, a domain
   as what it is made from -- and each disagreement is named. *)
let test_statements_are_checked_against_the_database () =
  on_db
    [
      "create type colour as enum ('black', 'white')";
      "create domain points as int4";
      "create table v (n int4, c colour, p points, at timestamptz, d bytea)";
    ] (fun db ->
      let right =
        [
          S.Any
            (S.find_opt ~params:S.int ~row:(S.t2 S.text S.int)
               "select c::colour::text, p from v where n = $1");
          S.Any
            (S.list ~params:S.unit
               ~row:(S.t3 S.text S.int Db_target.instant_us)
               "select c, p, at from v");
          S.Any
            (S.exec ~params:(S.t2 S.text S.bytes)
               "insert into v (c, d) values ($1::colour, $2)");
          S.Any (S.exec_count ~params:S.int "delete from v where n = $1");
        ]
      in
      Alcotest.(check (result unit (list string)))
        "agreeing, passes" (Ok ()) (Pg.verify db right);
      let wrong =
        [
          S.Any (S.list ~params:S.unit ~row:S.int "select c from v");
          S.Any (S.exec ~params:S.bool "delete from v where n = $1");
          S.Any (S.find_opt ~params:S.unit ~row:S.int "select n, p from v");
          S.Any (S.list ~params:S.unit ~row:S.int "selec n from v");
        ]
      in
      match Pg.verify db wrong with
      | Ok () -> Alcotest.fail "every disagreement passed"
      | Error ps ->
          Alcotest.(check int) "one problem each" 4 (List.length ps);
          List.iter2
            (fun p says -> Alcotest.(check bool) p true (contains p says))
            ps
            [
              "column 1 is colour, which int does not read";
              "parameter 1 is int4, which bool does not read";
              "1 columns declared, and the database has 2";
              "the database refuses it";
            ];
          (* A check that cannot ask is one failure, not one per column. *)
          Pg.close db;
          Alcotest.(check (result unit (list string)))
            "a closed connection, said once"
            (Error [ "the check could not run: the connection is closed" ])
            (Pg.verify db (right @ wrong)))

(* An array goes out as one parameter and comes back as a list, every
   element whatever it holds -- a comma, a quote, a backslash, a space, the
   word NULL -- and a NULL element only where it is one. *)
let test_arrays_round_trip () =
  on_db
    [
      "create table a (n smallint[], f real[], t text[], b bytea[], at \
       timestamptz[], k uuid[])";
    ] (fun db ->
      let texts =
        [ "a,b"; {|say "hi"|}; {|back\slash|}; " spaced "; "NULL"; "" ]
      in
      let bytes = [ "\x00\xff"; "" ] in
      let keys = [ "0190c0fe-0000-7000-8000-000000000001" ] in
      ok
        (Pg.run db
           (S.exec
              ~params:
                (S.t6 (S.array S.int)
                   (S.array (S.opt S.float))
                   (S.array S.text) (S.array S.bytes)
                   (S.array Db_target.instant_us)
                   (S.array S.text))
              "insert into a values ($1, $2, $3, $4, $5, $6)")
           ( [ 1; -2; 3 ],
             [ Some 1.5; None ],
             texts,
             bytes,
             [ 1_758_620_000_123_456 ],
             keys ));
      match
        ok
          (Pg.run db
             (S.find ~params:S.unit
                ~row:
                  (S.t6 (S.array S.int)
                     (S.array (S.opt S.float))
                     (S.array S.text) (S.array S.bytes)
                     (S.array Db_target.instant_us)
                     (S.array S.text))
                "select n, f, t, b, at, k from a")
             ())
      with
      | n, f, t, b, at, k ->
          Alcotest.(check (list int)) "integers" [ 1; -2; 3 ] n;
          Alcotest.(check (list (option (float 0.))))
            "floats, a NULL among them" [ Some 1.5; None ] f;
          Alcotest.(check (list string)) "text, whatever it holds" texts t;
          Alcotest.(check (list string)) "bytes" bytes b;
          Alcotest.(check (list int)) "instants" [ 1_758_620_000_123_456 ] at;
          Alcotest.(check (list string)) "keys" keys k)

(* An empty list is an empty array, and an array is a parameter wherever
   one is: [= any($1)] among them. *)
let test_an_array_is_a_parameter () =
  on_db [ "create table e (n int4)"; "insert into e values (1), (2), (3)" ]
    (fun db ->
      let among =
        S.list ~params:(S.array S.int) ~row:S.int
          "select n from e where n = any($1) order by n"
      in
      Alcotest.(check (list int))
        "any of them" [ 1; 3 ]
        (ok (Pg.run db among [ 3; 1 ]));
      Alcotest.(check (list int))
        "and of none, none" []
        (ok (Pg.run db among []));
      Alcotest.(check (list int))
        "an empty one reads back" []
        (ok
           (Pg.run db
              (S.find ~params:S.unit ~row:(S.array S.int) "select '{}'::int4[]")
              ())))

(* An element is one column: a shape of more is the statement's error, and
   nothing is sent. *)
let test_an_element_of_two_columns_is_refused () =
  on_db [] (fun db ->
      match
        Pg.run db
          (S.find ~params:(S.array (S.t2 S.int S.int)) ~row:S.int "select 1")
          [ (1, 2) ]
      with
      | Error (`Db m) -> Alcotest.(check bool) m true (contains m "one column")
      | Error (`Conflict _ | `Not_serializable _) ->
          Alcotest.fail "not the shape's error"
      | Ok _ -> Alcotest.fail "an array of pairs was sent")

(* The check reads an array by its element's type. *)
let test_arrays_are_checked_by_their_elements () =
  on_db [ "create table c (n smallint[], t text[])" ] (fun db ->
      Alcotest.(check (result unit (list string)))
        "agreeing" (Ok ())
        (Pg.verify db
           [
             S.Any
               (S.list ~params:(S.array S.text)
                  ~row:(S.t2 (S.array S.int) (S.array S.text))
                  "select n, t from c where t = $1");
           ]);
      let one_problem what row sql says =
        match Pg.verify db [ S.Any (S.list ~params:S.unit ~row sql) ] with
        | Error [ p ] -> Alcotest.(check bool) p true (contains p says)
        | Error ps -> Alcotest.failf "%s: %d problems" what (List.length ps)
        | Ok () -> Alcotest.failf "%s passed" what
      in
      one_problem "an array of text read as integers" (S.array S.int)
        "select t from c" "an array of int does not read";
      one_problem "an array of arrays"
        (S.array (S.array S.int))
        "select n from c" "no array of its own")

(* An instant goes out as UTC and comes back from Postgres' own output,
   whatever the session's time zone -- including one with a half-hour
   offset -- to the microsecond, before the epoch as after it. *)
let test_an_instant_round_trips () =
  on_db [ "create table i (t timestamptz)"; "set time zone 'Asia/Kolkata'" ]
    (fun db ->
      let moments =
        [
          0; 1_758_620_000_123_456; 1; -1; 951_782_400_000_000; -86_400_000_001;
        ]
      in
      List.iter
        (fun us ->
          ok (Pg.exec_raw db "delete from i");
          ok
            (Pg.run db
               (S.exec ~params:Db_target.instant_us "insert into i values ($1)")
               us);
          Alcotest.(check (option int))
            (string_of_int us) (Some us)
            (ok
               (Pg.run db
                  (S.find_opt ~params:S.unit ~row:Db_target.instant_us
                     "select t from i")
                  ())))
        moments;
      Alcotest.(check (option bool))
        "and the database agrees about which instant" (Some true)
        (ok
           (Pg.run db
              (S.find_opt ~params:Db_target.instant_us ~row:S.bool
                 "select $1::timestamptz = timestamptz '2025-09-23 \
                  09:33:20.123456+00'")
              1_758_620_000_123_456)))

(* A uuid and a JSON document are each their own type: written as their
   text, read back as Postgres writes them, and checked against the
   column's type. *)
let test_uuid_and_json_round_trip () =
  on_db [ "create table d (id uuid, doc json, data jsonb)" ] (fun db ->
      let id = "0190c0fe-7a1b-7c3d-8e4f-a0b1c2d3e4f5"
      and doc = {|{"b": [1, 2], "a": "x"}|} in
      let insert =
        S.exec
          ~params:S.(t3 Db_target.uuid_text json json)
          "insert into d values ($1, $2, $3)"
      and select =
        S.find ~params:S.unit
          ~row:S.(t3 Db_target.uuid_text json json)
          "select id, doc, data from d"
      in
      ok (Pg.run db insert (String.uppercase_ascii id, doc, doc));
      let read_id, read_doc, read_data = ok (Pg.run db select ()) in
      Alcotest.(check string) "a uuid, lowercase" id read_id;
      Alcotest.(check string) "json, as it was written" doc read_doc;
      Alcotest.(check string)
        "jsonb, as Postgres keeps it" {|{"a": "x", "b": [1, 2]}|} read_data;
      match Pg.verify db [ S.Any insert; S.Any select ] with
      | Ok () -> ()
      | Error ps -> Alcotest.failf "refused: %s" (String.concat "; " ps))

let test_arity () =
  Alcotest.(check int) "scalar" 1 (S.arity S.int);
  Alcotest.(check int) "unit" 0 (S.arity S.unit);
  Alcotest.(check int) "t3" 3 S.(arity (t3 int text float));
  Alcotest.(check int) "option is transparent" 1 S.(arity (opt int));
  Alcotest.(check int)
    "nested option" 4
    S.(arity (t3 int text (opt (t2 int int))))

let test_conv_record () =
  on_db [ "create table p (x integer, y integer)" ] (fun db ->
      let shape =
        S.conv (S.t2 S.int S.int)
          ~of_:(fun (x, y) -> (x, y))
          ~to_:(fun (x, y) -> (x, y))
      in
      ok
        (Pg.run db
           (S.exec ~params:shape "insert into p values ($1, $2)")
           (3, 4));
      Alcotest.(check (option (pair int int)))
        "conv roundtrip"
        (Some (3, 4))
        (ok
           (Pg.run db
              (S.find_opt ~params:S.unit ~row:shape "select x, y from p")
              ())))

(* A connection the server dropped -- a restart, a failover, an idle kill --
   answers its next statement with an error like any other, and is usable
   again once revived. *)
let test_a_lost_connection_is_an_error () =
  on_db [] (fun db ->
      let pid =
        ok
          (Pg.run db
             (S.find_opt ~params:S.unit ~row:S.int "select pg_backend_pid()")
             ())
      in
      Db_target.admin (fun admin ->
          match pid with
          | None -> Alcotest.fail "no backend pid"
          | Some pid ->
              ignore
                (ok
                   (Pg.run admin
                      (S.list ~params:S.int ~row:S.bool
                         "select pg_terminate_backend($1)")
                      pid)
                  : bool list));
      (match Pg.exec_raw db "select 1" with
      | Error (`Db _ | `Not_serializable _) -> ()
      | Error (`Conflict _) ->
          Alcotest.fail "a lost connection is not a conflict"
      | Ok () -> Alcotest.fail "a statement on a dropped connection succeeded");
      Pg.revive db;
      ok (Pg.exec_raw db "select 1"))

(* ------------------------------------------------------------------ *)
(* Transactions *)

module T = Rowtype_postgres.Transaction

let committed db =
  List.length
    (ok (Pg.run db (S.list ~params:S.unit ~row:S.int "select n from t") ()))

let insert db n = Pg.run db (S.exec ~params:S.int "insert into t values ($1)") n

exception Work_raised

(* A raise inside the work is a bug, not an answer: the transaction rolls
   back, the raise passes, and the connection reads on. *)
let test_a_raise_rolls_back_and_passes () =
  on_db [ "create table t (n int)" ] (fun db ->
      (match
         T.within db (fun db ->
             ignore (ok (insert db 1));
             raise Work_raised)
       with
      | exception Work_raised -> ()
      | Ok () | Error _ -> Alcotest.fail "the raise did not pass");
      Alcotest.(check int) "nothing was kept" 0 (committed db);
      ok (insert db 2);
      Alcotest.(check int) "and the connection writes on" 1 (committed db))

(* A transaction inside another on one connection would have its COMMIT
   end the outer one too, which Postgres only warns about: it is refused
   before it begins, and the outer one goes on whole. *)
let test_a_transaction_inside_another_is_refused () =
  on_db [ "create table t (n int)" ] (fun db ->
      let inner = ref None in
      (match
         T.within db (fun db ->
             ignore (ok (insert db 1));
             inner := Some (T.within db (fun db -> insert db 2));
             insert db 3)
       with
      | Ok () -> ()
      | Error _ -> Alcotest.fail "the outer transaction failed");
      (match !inner with
      | Some (Error (`Not_committed _)) -> ()
      | Some (Ok ()) -> Alcotest.fail "the inner transaction ran"
      | Some (Error _) | None ->
          Alcotest.fail "the inner transaction was not refused");
      Alcotest.(check int)
        "the outer one's rows, and only those" 2 (committed db))

(* A connection the server dropped is made again before the transaction
   begins, the one point where starting again loses nothing. *)
let test_a_dropped_connection_is_revived_before_begin () =
  on_db [ "create table t (n int)" ] (fun db ->
      let pid =
        ok
          (Pg.run db
             (S.find ~params:S.unit ~row:S.int "select pg_backend_pid()")
             ())
      in
      Db_target.admin (fun admin ->
          ignore
            (ok
               (Pg.run admin
                  (S.find ~params:S.int ~row:S.bool
                     "select pg_terminate_backend($1)")
                  pid)
              : bool));
      (match T.within db (fun db -> insert db 1) with
      | Ok () -> ()
      | Error (#S.error as e) ->
          Alcotest.failf "refused: %s" (S.error_to_string e)
      | Error (`Not_committed m) -> Alcotest.failf "not committed: %s" m);
      Alcotest.(check int) "the write landed" 1 (committed db))

(* A connection lost inside a transaction last heard it was in one; the
   next transaction revives it rather than refusing it as nested. *)
let test_a_connection_lost_inside_a_transaction_is_revived () =
  on_db [ "create table t (n int)" ] (fun db ->
      let lost =
        T.within db (fun db ->
            let pid =
              ok
                (Pg.run db
                   (S.find ~params:S.unit ~row:S.int "select pg_backend_pid()")
                   ())
            in
            Db_target.admin (fun admin ->
                ignore
                  (ok
                     (Pg.run admin
                        (S.find ~params:S.int ~row:S.bool
                           "select pg_terminate_backend($1)")
                        pid)
                    : bool));
            insert db 1)
      in
      (match lost with
      | Error _ -> ()
      | Ok () -> Alcotest.fail "a write on a lost connection succeeded");
      (match T.within db (fun db -> insert db 2) with
      | Ok () -> ()
      | Error (#S.error as e) ->
          Alcotest.failf "refused: %s" (S.error_to_string e)
      | Error (`Not_committed m) -> Alcotest.failf "not committed: %s" m);
      Alcotest.(check int) "the second write landed" 1 (committed db))

(* A COMMIT can fail on its own: a deferred constraint is checked there.
   The work's Ok must not survive it, because nothing it wrote did. *)
let test_a_failed_commit_is_not_committed () =
  on_db
    [
      "create table t (n int, constraint u unique (n) deferrable initially \
       deferred)";
      "insert into t values (1)";
    ] (fun db ->
      (match T.within db (fun db -> insert db 1) with
      | Error (`Not_committed _) -> ()
      | Error (`Conflict _ | `Db _ | `Not_serializable _) ->
          Alcotest.fail "the insert itself was refused"
      | Ok () -> Alcotest.fail "a failed commit answered success");
      Alcotest.(check int) "nothing was kept" 1 (committed db))

(* Postgres rolls an aborted transaction back without an error, so work
   that swallowed the failed statement would otherwise answer success. *)
let test_ok_from_an_aborted_transaction_is_not_committed () =
  on_db [ "create table t (n int primary key)"; "insert into t values (1)" ]
    (fun db ->
      match
        T.within db (fun db ->
            ignore
              (Pg.run db (S.exec ~params:S.int "insert into t values ($1)") 1
                : (unit, S.error) result);
            Ok ())
      with
      | Error (`Not_committed _) -> ()
      | Error (`Not_serializable m) -> Alcotest.failf "not the commit's: %s" m
      | Ok () -> Alcotest.fail "a swallowed failure answered success")

(* The warnings logged while [f] runs. *)
let warnings f =
  let lines = ref [] in
  let report _src level ~over k msgf =
    msgf (fun ?header:_ ?tags:_ fmt ->
        Format.kasprintf
          (fun s ->
            (match level with
            | Logs.Warning -> lines := s :: !lines
            | Logs.App | Logs.Error | Logs.Info | Logs.Debug -> ());
            over ();
            k ())
          fmt)
  in
  let reporter = Logs.reporter () and level = Logs.level () in
  Logs.set_reporter { Logs.report };
  Logs.set_level (Some Logs.Warning);
  Fun.protect
    ~finally:(fun () ->
      Logs.set_reporter reporter;
      Logs.set_level level)
    f;
  List.rev !lines

(* ...while the same failure RETURNED is the work's own answer: a race lost
   to a unique index is a refusal, not a transaction gone wrong -- and a
   refusal that asked to keep its write is told it was not kept. *)
let test_a_returned_failure_is_the_works_answer () =
  on_db [ "create table t (n int primary key)"; "insert into t values (1)" ]
    (fun db ->
      let said =
        warnings (fun () ->
            match T.within ~keep:(fun _ -> true) db (fun db -> insert db 1) with
            | Error (`Conflict _) -> ()
            | Error (`Db m | `Not_serializable m) ->
                Alcotest.failf "not a conflict: %s" m
            | Error (`Not_committed m) -> Alcotest.failf "not the work's: %s" m
            | Ok () -> Alcotest.fail "the duplicate was accepted")
      in
      Alcotest.(check bool)
        "the rollback is said" true
        (List.exists
           (fun l ->
             String.length l >= 10
             && String.equal (String.sub l 0 10) "a refusal ")
           said))

(* A refusal keeps nothing unless the work says it keeps what it wrote. *)
let test_a_refusal_rolls_back_unless_it_says () =
  on_db [ "create table t (n int)" ] (fun db ->
      let refuse ?keep () =
        match
          T.within ?keep db (fun db ->
              Result.bind (insert db 1) (fun () -> Error `No))
        with
        | Error `No -> ()
        | Error (`Conflict _ | `Db _ | `Not_serializable _ | `Not_committed _)
          ->
            Alcotest.fail "not the refusal"
        | Ok () -> Alcotest.fail "not refused"
      in
      refuse ();
      Alcotest.(check int) "rolled back" 0 (committed db);
      refuse ~keep:(function `Conflict _ -> true | _ -> false) ();
      Alcotest.(check int) "another refusal kept: not this one" 0 (committed db);
      refuse ~keep:(function `No -> true | _ -> false) ();
      Alcotest.(check int) "said to keep it: kept" 1 (committed db))

(* A transaction's answer, which the test expects to have landed. *)
let landed = function
  | Ok v -> v
  | Error (#S.error as e) -> Alcotest.fail (S.error_to_string e)
  | Error (`Not_committed m) -> Alcotest.failf "not committed: %s" m

(* The level a transaction begins at is the one it was given, and the
   database's own default when none was. *)
(* An observer is handed each statement's text -- a transaction's [begin]
   and [commit] among them, and never a value -- and answers what the
   statement did; a pool hands its observer to every connection it lends. *)
let test_an_observer_sees_every_statement () =
  Db_target.with_postgres (fun target ->
      let seen = ref [] in
      let observe =
        {
          Pg.around =
            (fun sql run ->
              seen := sql :: !seen;
              run ());
        }
      in
      let length db =
        Pg.run db
          (S.find ~params:S.text ~row:S.int "select length($1)::int")
          "hunter2"
      in
      let c = ok (Pg.conninfo target) in
      let db =
        ok
          (Pg.connect ~sw:(Db_target.sw ()) ~net:(Db_target.net ())
             ~mono_clock:(Db_target.mono ()) ~observe c)
      in
      Fun.protect
        ~finally:(fun () -> Pg.close db)
        (fun () ->
          Alcotest.(check int)
            "what the statement answered" 7
            (landed (T.within db length)));
      Alcotest.(check (list string))
        "every statement, by its text"
        [ "begin"; "select length($1)::int"; "commit" ]
        (List.rev !seen);
      seen := [];
      let pool =
        ok
          (Pg.Pool.create ~sw:(Db_target.sw ()) ~net:(Db_target.net ())
             ~mono_clock:(Db_target.mono ()) ~size:1 ~observe c)
      in
      Fun.protect
        ~finally:(fun () -> Pg.Pool.close pool)
        (fun () ->
          match Pg.Pool.use pool length with
          | Ok (Ok 7) -> ()
          | Ok _ | Error _ -> Alcotest.fail "the pool's statement");
      Alcotest.(check (list string))
        "and a pool's connections"
        [ "select length($1)::int" ]
        !seen)

let test_a_transaction_begins_at_its_level () =
  on_db [] (fun db ->
      let level ?isolation () =
        landed
          (T.within ?isolation db (fun db ->
               Pg.run db
                 (S.find ~params:S.unit ~row:S.text "show transaction_isolation")
                 ()))
      in
      Alcotest.(check string) "unless given" "read committed" (level ());
      Alcotest.(check string)
        "repeatable read" "repeatable read"
        (level ~isolation:T.Repeatable_read ());
      Alcotest.(check string)
        "serializable" "serializable"
        (level ~isolation:T.Serializable ()))

(* A statement Postgres could not order with a concurrent one -- 40001, and
   a deadlock, 40P01 -- is its own failure, because running the transaction
   again may get past it and nothing else here may. *)
let test_a_serialization_failure_is_its_own () =
  on_db [] (fun db ->
      List.iter
        (fun code ->
          match
            Pg.exec_raw db
              (Printf.sprintf
                 "do $$ begin raise exception using errcode = '%s'; end $$" code)
          with
          | Error (`Not_serializable _) -> ()
          | Error (`Conflict _ | `Db _) -> Alcotest.failf "%s: not its own" code
          | Ok () -> Alcotest.failf "%s: nothing was raised" code)
        [ "40001"; "40P01" ])

(* [flaky ()] refuses as not serializable the first [n] times it is called
   in the database's life -- a sequence is not rolled back -- and then
   writes a row. *)
let flaky_setup n =
  [
    "create table t (n int)";
    "create sequence tries";
    Printf.sprintf
      "create function flaky() returns void language plpgsql as $$ begin if \
       nextval('tries') <= %d then raise exception using errcode = '40001'; \
       end if; insert into t values (1); end $$"
      n;
  ]

let tries db =
  ok
    (Pg.run db
       (S.find ~params:S.unit ~row:S.int "select last_value::int from tries")
       ())

let flaky db = Pg.exec_raw db "select flaky()"

(* Retried, the whole transaction runs again and lands once; not retried,
   or retried too few times, the failure is the answer and nothing lands. *)
let test_retries_run_the_transaction_again () =
  on_db (flaky_setup 2) (fun db ->
      (match T.within db flaky with
      | Error (`Not_serializable _) -> ()
      | Error (`Conflict _ | `Db _ | `Not_committed _) ->
          Alcotest.fail "not the serialization failure"
      | Ok () -> Alcotest.fail "the first attempt landed");
      Alcotest.(check int) "unless asked, once" 1 (tries db);
      landed (T.within ~retries:3 db flaky);
      Alcotest.(check int) "then until it lands" 3 (tries db);
      Alcotest.(check int) "and it lands once" 1 (committed db))

(* A COMMIT can find the transaction not serializable as well -- here a
   deferred trigger that refuses the first time -- and that is the same
   failure, retried the same way, never [`Not_committed]. *)
let test_a_commit_not_serializable_is_retried () =
  on_db
    [
      "create table t (n int)";
      "create sequence tries";
      "create function refuse_once() returns trigger language plpgsql as $$ \
       begin if nextval('tries') <= 1 then raise exception using errcode = \
       '40001'; end if; return null; end $$";
      "create constraint trigger at_commit after insert on t deferrable \
       initially deferred for each row execute function refuse_once()";
    ] (fun db ->
      let add db = insert db 1 in
      (match T.within db add with
      | Error (`Not_serializable _) -> ()
      | Error (`Not_committed m) -> Alcotest.failf "said not committed: %s" m
      | Error (`Conflict _ | `Db _) -> Alcotest.fail "the insert was refused"
      | Ok () -> Alcotest.fail "a refused commit answered success");
      Alcotest.(check int) "nothing kept" 0 (committed db);
      landed (T.within ~retries:1 db add);
      Alcotest.(check int) "run again, it lands" 1 (committed db))

(* A refusal kept by [keep] commits like an [Ok], so its COMMIT refused as
   not serializable is retried like one, and the refusal kept once it
   lands. *)
let test_a_kept_refusal_not_serializable_is_retried () =
  on_db
    [
      "create table t (n int)";
      "create sequence tries";
      "create function refuse_once() returns trigger language plpgsql as $$ \
       begin if nextval('tries') <= 1 then raise exception using errcode = \
       '40001'; end if; return null; end $$";
      "create constraint trigger at_commit after insert on t deferrable \
       initially deferred for each row execute function refuse_once()";
    ] (fun db ->
      let counted db =
        Result.bind (insert db 1) (fun () -> Error `Wrong_code)
      in
      let keep = function `Wrong_code -> true | _ -> false in
      (match T.within ~keep ~retries:1 db counted with
      | Error `Wrong_code -> ()
      | Error (`Not_serializable _) -> Alcotest.fail "not run again"
      | Error (`Not_committed m) -> Alcotest.failf "said not committed: %s" m
      | Error (`Conflict _ | `Db _) -> Alcotest.fail "the insert was refused"
      | Ok () -> Alcotest.fail "a refusal answered success");
      Alcotest.(check int) "its write kept, once" 1 (committed db))

(* How many values a statement asks for is the database's to check: a
   shape that binds a number the statement does not ask for is refused by
   the server, and nothing runs. *)
let test_a_count_that_disagrees_is_refused_by_the_server () =
  on_db [] (fun db ->
      Alcotest.(check bool)
        "two parameters for one" true
        (match
           Pg.run db
             (S.find_opt ~params:S.(t2 int int) ~row:S.int "select $1::int")
             (1, 2)
         with
        | Error (`Db _ | `Not_serializable _) -> true
        | Error (`Conflict _) | Ok _ -> false);
      Alcotest.(check bool)
        "none for one" true
        (match
           Pg.run db (S.find_opt ~params:S.unit ~row:S.int "select $1::int") ()
         with
        | Error (`Db _ | `Not_serializable _) -> true
        | Error (`Conflict _) | Ok _ -> false))

(* A NULL is not empty text: text refuses one like every other scalar,
   and a column that may hold one is [opt]'s. *)
let test_a_null_is_not_empty_text () =
  on_db [ "create table n (a text)"; "insert into n values (null)" ] (fun db ->
      (match
         Pg.run db (S.find_opt ~params:S.unit ~row:S.text "select a from n") ()
       with
      | Error (`Db m | `Not_serializable m) ->
          Alcotest.(check bool) "said as NULL" true (contains m "NULL")
      | Ok _ | Error (`Conflict _) -> Alcotest.fail "a NULL read as text");
      Alcotest.(check (option (option string)))
        "and as None where it may be one" (Some None)
        (ok
           (Pg.run db
              (S.find_opt ~params:S.unit ~row:S.(opt text) "select a from n")
              ())))

(* A shape wider than the result is the caller's error, said as one rather
   than raised by the binding. *)
let test_a_shape_wider_than_the_result () =
  on_db [] (fun db ->
      match
        Pg.run db (S.find_opt ~params:S.unit ~row:S.(t2 int int) "select 1") ()
      with
      | Error (`Db m | `Not_serializable m) ->
          Alcotest.(check bool)
            "the column it lacks" true (contains m "column 1")
      | Ok _ | Error (`Conflict _) -> Alcotest.fail "decoded a column not there")

(* Another database is the same server with another name, through the
   connection string's own reading: a URL's query kept, a URL with no path
   given one, and a keyword list's quoted value kept whole. *)
let test_another_database_keeps_its_server () =
  let on server name =
    match Pg.on_database ~server name with
    | Ok url -> url
    | Error e -> Alcotest.fail (S.error_to_string e)
  in
  Alcotest.(check string)
    "the query kept" "postgres://u:p@h:5432/t_1?sslmode=require"
    (on "postgres://u:p@h:5432/main?sslmode=require" "t_1");
  Alcotest.(check string)
    "a path given" "postgres://u@h:5432/t_1?sslmode=prefer"
    (on "postgres://u@h" "t_1");
  Alcotest.(check string)
    "a keyword list's dbname, and a quoted password"
    "postgres://u:a%20b@h:5432/t_1?sslmode=prefer"
    (on "host=h dbname=main user=u password='a b'" "t_1")

let () =
  Db_target.required ~suite:"rowtype";
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Db_target.eio env ~sw;
  Alcotest.run ~and_exit:false "rowtype"
    [
      ( "shapes",
        [
          Alcotest.test_case "arity" `Quick test_arity;
          Alcotest.test_case "conv" `Quick test_conv_record;
        ] );
      ( "roundtrip",
        [
          Alcotest.test_case "t4 with option" `Quick test_roundtrip_t4;
          Alcotest.test_case "an instant" `Quick test_an_instant_round_trips;
          Alcotest.test_case "a uuid and JSON" `Quick
            test_uuid_and_json_round_trip;
          Alcotest.test_case "bytes" `Quick test_bytes_round_trip;
          Alcotest.test_case "arrays" `Quick test_arrays_round_trip;
          Alcotest.test_case "an array is a parameter" `Quick
            test_an_array_is_a_parameter;
          Alcotest.test_case "an element of two columns is refused" `Quick
            test_an_element_of_two_columns_is_refused;
          Alcotest.test_case "arrays are checked by their elements" `Quick
            test_arrays_are_checked_by_their_elements;
          Alcotest.test_case "statements are checked against the database"
            `Quick test_statements_are_checked_against_the_database;
          Alcotest.test_case "NULL as None" `Quick test_option_null;
          Alcotest.test_case "find_opt" `Quick test_find_opt;
          Alcotest.test_case "float accepts int" `Quick test_float_accepts_int;
          Alcotest.test_case "exec_count counts rows" `Quick
            test_exec_count_counts_rows;
        ] );
      ( "placeholders",
        [
          Alcotest.test_case "order is preserved" `Quick test_placeholder_order;
          Alcotest.test_case "a parameter used twice" `Quick
            test_a_parameter_used_twice;
          Alcotest.test_case "a ? is Postgres's" `Quick
            test_a_question_mark_is_postgres_s;
          Alcotest.test_case "a count that disagrees is refused by the server"
            `Quick test_a_count_that_disagrees_is_refused_by_the_server;
        ] );
      ( "transactions",
        [
          Alcotest.test_case "a failed commit is not committed" `Quick
            test_a_failed_commit_is_not_committed;
          Alcotest.test_case "Ok from an aborted transaction is not committed"
            `Quick test_ok_from_an_aborted_transaction_is_not_committed;
          Alcotest.test_case "a returned failure is the work's answer" `Quick
            test_a_returned_failure_is_the_works_answer;
          Alcotest.test_case "a refusal rolls back unless it says" `Quick
            test_a_refusal_rolls_back_unless_it_says;
          Alcotest.test_case "a transaction begins at its level" `Quick
            test_a_transaction_begins_at_its_level;
          Alcotest.test_case "a raise rolls back and passes" `Quick
            test_a_raise_rolls_back_and_passes;
          Alcotest.test_case "a transaction inside another is refused" `Quick
            test_a_transaction_inside_another_is_refused;
          Alcotest.test_case "a dropped connection is revived before begin"
            `Quick test_a_dropped_connection_is_revived_before_begin;
          Alcotest.test_case "a connection lost in a transaction is revived"
            `Quick test_a_connection_lost_inside_a_transaction_is_revived;
          Alcotest.test_case "an observer sees every statement" `Quick
            test_an_observer_sees_every_statement;
          Alcotest.test_case "a serialization failure is its own" `Quick
            test_a_serialization_failure_is_its_own;
          Alcotest.test_case "retries run the transaction again" `Quick
            test_retries_run_the_transaction_again;
          Alcotest.test_case "a commit not serializable is retried" `Quick
            test_a_commit_not_serializable_is_retried;
          Alcotest.test_case "a kept refusal not serializable is retried" `Quick
            test_a_kept_refusal_not_serializable_is_retried;
          Alcotest.test_case "another database keeps its server" `Quick
            test_another_database_keeps_its_server;
        ] );
      ( "errors",
        [
          Alcotest.test_case "a lost connection is an error" `Quick
            test_a_lost_connection_is_an_error;
          Alcotest.test_case "conflict is distinct" `Quick
            test_conflict_is_distinct;
          Alcotest.test_case "conflict through returning" `Quick
            test_conflict_through_returning;
          Alcotest.test_case "a conflict names its constraint" `Quick
            test_a_conflict_names_its_constraint;
          Alcotest.test_case "a conflict is about other rows" `Quick
            test_a_conflict_is_about_other_rows;
          Alcotest.test_case "find is exactly one row" `Quick
            test_find_is_exactly_one_row;
          Alcotest.test_case "a fold is run without the list" `Quick
            test_fold_is_run_without_the_list;
          Alcotest.test_case "a fold stops at a row it cannot read" `Quick
            test_a_fold_stops_at_a_row_it_cannot_read;
          Alcotest.test_case "type mismatch" `Quick test_type_mismatch_reported;
          Alcotest.test_case "an unreadable value" `Quick
            test_parse_refuses_what_it_cannot_read;
          Alcotest.test_case "a NULL is not empty text" `Quick
            test_a_null_is_not_empty_text;
          Alcotest.test_case "a shape wider than the result" `Quick
            test_a_shape_wider_than_the_result;
        ] );
    ]

(* The SQLite backend against SQLite itself: a database in memory for a
   case that needs one connection, and a file of the case's own for one
   that needs two. Nothing here waits on a clock: a lock held for a case
   is held until the case lets it go. *)

module S = Rowtype
module Sq = Rowtype_sqlite

let ok = function
  | Ok v -> v
  | Error e -> Alcotest.failf "unexpected error: %s" (Sq.error_to_string e)

let contains haystack needle =
  let n = String.length needle and h = String.length haystack in
  let rec go i =
    i + n <= h && (String.equal (String.sub haystack i n) needle || go (i + 1))
  in
  go 0

let db_error = function
  | Error (`Db m) -> m
  | Ok _ -> Alcotest.fail "expected a `Db error, got Ok"
  | Error e ->
      Alcotest.failf "expected a `Db error, got %s" (Sq.error_to_string e)

(* A file no other case shares, and the journal files SQLite makes beside
   it, removed after. *)
let with_file f =
  let file = Filename.temp_file "rowtype_sqlite" ".db" in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun suffix ->
          match Sys.remove (file ^ suffix) with
          | () -> ()
          | exception Sys_error _ -> ())
        [ ""; "-journal"; "-wal"; "-shm" ])
    (fun () -> f file)

let with_eio f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw -> f ~sw ~mono_clock:(Eio.Stdenv.mono_clock env)

let on_db setup f =
  with_eio @@ fun ~sw ~mono_clock ->
  let db = ok (Sq.connect ~sw ~mono_clock ":memory:") in
  List.iter (fun s -> ok (Sq.exec_raw db s)) setup;
  f db

(* Two connections to one file, each waiting [timeout] for a lock. *)
let on_two ?timeout setup f =
  with_file @@ fun file ->
  with_eio @@ fun ~sw ~mono_clock ->
  let a = ok (Sq.connect ~sw ~mono_clock ?timeout file) in
  let b = ok (Sq.connect ~sw ~mono_clock ?timeout file) in
  List.iter (fun s -> ok (Sq.exec_raw a s)) setup;
  f a b

let some = function Some v -> v | None -> Alcotest.fail "expected a value"

(* [us] microseconds after [t]. *)
let after t us =
  some
    (Ptime.add_span t
       (Ptime.Span.v (0, Int64.mul (Int64.of_int us) 1_000_000L)))

let utc y mo d h mi s = some (Ptime.of_date_time ((y, mo, d), ((h, mi, s), 0)))

let count db table =
  ok
    (Sq.run db
       (S.find ~params:S.unit ~row:S.int ("select count(*) from " ^ table))
       ())

(* ------------------------------------------------------------------ *)
(* Statements *)

let test_roundtrip_t4 () =
  on_db [ "create table t (a integer, b real, c text, d integer)" ] (fun db ->
      let shape = S.(t4 int float text (opt int)) in
      ok
        (Sq.run db
           (S.exec ~params:shape "insert into t values ($1, $2, $3, $4)")
           (42, 1.5, "hi", Some 7));
      ok
        (Sq.run db
           (S.exec ~params:shape "insert into t values ($1, $2, $3, $4)")
           (43, 2.5, "ho", None));
      match
        ok
          (Sq.run db
             (S.list ~params:S.unit ~row:shape
                "select a, b, c, d from t order by a")
             ())
      with
      | [ (42, 1.5, "hi", Some 7); (43, 2.5, "ho", None) ] -> ()
      | rows -> Alcotest.failf "unexpected rows (%d)" (List.length rows))

let test_find () =
  on_db
    [
      "create table t (k integer primary key, v text)";
      "insert into t values (1, 'one'), (2, 'two')";
    ] (fun db ->
      let one =
        S.find ~params:S.int ~row:S.text "select v from t where k = $1"
      in
      Alcotest.(check string) "one row" "one" (ok (Sq.run db one 1));
      Alcotest.(check bool)
        "no row is an error saying so" true
        (contains (db_error (Sq.run db one 9)) "answered 0 rows");
      let opt =
        S.find_opt ~params:S.int ~row:S.text "select v from t where k = $1"
      in
      Alcotest.(check (option string)) "none" None (ok (Sq.run db opt 9));
      Alcotest.(check bool)
        "several is an error saying how many" true
        (contains
           (db_error
              (Sq.run db
                 (S.find_opt ~params:S.unit ~row:S.text "select v from t")
                 ()))
           "answered 2 rows"))

let test_exec_count () =
  on_db [ "create table t (k integer, v text)" ] (fun db ->
      ok
        (Sq.run_many db
           (S.exec ~params:S.int "insert into t (k) values ($1)")
           [ 1; 2; 3; 4 ]);
      Alcotest.(check int)
        "rows an update changed" 3
        (ok
           (Sq.run db
              (S.exec_count ~params:S.int "update t set v = 'x' where k > $1")
              1));
      Alcotest.(check int)
        "rows an insert returning changed" 2
        (ok
           (Sq.run db
              (S.exec_count ~params:S.unit
                 "insert into t (k) values (5), (6) returning k")
              ()));
      Alcotest.(check int)
        "rows a select sent" 6
        (ok (Sq.run db (S.exec_count ~params:S.unit "select k from t") ()));
      Alcotest.(check int)
        "rows a delete changed" 1
        (ok
           (Sq.run db
              (S.exec_count ~params:S.int "delete from t where k = $1")
              6)))

(* More rows than one chunk, so the fold goes on past a hop to the system
   thread, in order. *)
let test_fold_many () =
  on_db [ "create table t (k integer)" ] (fun db ->
      ok
        (Sq.exec_raw db
           "with recursive n(i) as (select 1 union all select i + 1 from n \
            where i < 1000) insert into t select i from n");
      let all = S.list ~params:S.unit ~row:S.int "select k from t order by k" in
      Alcotest.(check int)
        "every row, in order" 1000
        (ok
           (Sq.fold db all () ~init:0 (fun last k ->
                if k <> last + 1 then Alcotest.failf "row %d after %d" k last;
                k)));
      Alcotest.(check int)
        "the list too" 1000
        (List.length (ok (Sq.run db all ()))))

exception Stop

let test_fold_raise () =
  on_db [ "create table t (k integer)"; "insert into t values (1), (2), (3)" ]
    (fun db ->
      let all = S.list ~params:S.unit ~row:S.int "select k from t" in
      (match Sq.fold db all () ~init:() (fun () _ -> raise Stop) with
      | _ -> Alcotest.fail "the raise did not pass"
      | exception Stop -> ());
      Alcotest.(check int) "the connection goes on" 3 (count db "t"))

let test_run_many_all_or_nothing () =
  on_db [ "create table t (k integer primary key)" ] (fun db ->
      let insert = S.exec ~params:S.int "insert into t values ($1)" in
      (match Sq.run_many db insert [ 1; 2; 1; 3 ] with
      | Error (`Conflict (Some "t.k")) -> ()
      | Error e -> Alcotest.failf "unexpected error: %s" (Sq.error_to_string e)
      | Ok () -> Alcotest.fail "a repeated key was written");
      Alcotest.(check int) "none applied" 0 (count db "t");
      ok (Sq.run_many db insert [ 1; 2 ]);
      ok (Sq.run_many db insert []);
      Alcotest.(check int) "every one applied" 2 (count db "t");
      Alcotest.(check bool)
        "a statement that answers a row is refused" true
        (contains
           (db_error
              (Sq.run_many db
                 (S.find ~params:S.int ~row:S.unit
                    "insert into t values ($1) returning 1")
                 [ 9 ]))
           "run alone"))

(* A batch that fails inside a transaction undoes itself and leaves the
   transaction's own writes. *)
let test_run_many_in_transaction () =
  on_db [ "create table t (k integer primary key)" ] (fun db ->
      let insert = S.exec ~params:S.int "insert into t values ($1)" in
      ok
        (Sq.Transaction.within db (fun db ->
             let ( let* ) = Result.bind in
             let* () = Sq.run db insert 1 in
             (match Sq.run_many db insert [ 2; 1 ] with
             | Error (`Conflict _) -> ()
             | _ -> Alcotest.fail "the batch was not refused");
             Ok ()));
      Alcotest.(check int) "the transaction's own row" 1 (count db "t"))

(* ------------------------------------------------------------------ *)
(* Placeholders *)

let test_placeholders () =
  on_db [] (fun db ->
      let pair sql = S.find ~params:S.(t2 int text) ~row:S.(t2 text int) sql in
      Alcotest.(check (pair string int))
        "$2 before $1" ("a", 1)
        (ok (Sq.run db (pair "select $2, $1") (1, "a")));
      Alcotest.(check (pair string int))
        "?2 before ?1" ("a", 1)
        (ok (Sq.run db (pair "select ?2, ?1") (1, "a")));
      Alcotest.(check int)
        "$1 twice" 14
        (ok (Sq.run db (S.find ~params:S.int ~row:S.int "select $1 + $1") 7));
      Alcotest.(check (pair int string))
        "? in order" (1, "a")
        (ok
           (Sq.run db
              (S.find
                 ~params:S.(t2 int text)
                 ~row:S.(t2 int text)
                 "select ?, ?")
              (1, "a")));
      Alcotest.(check bool)
        "a named one is refused" true
        (contains
           (db_error
              (Sq.run db (S.find ~params:S.int ~row:S.int "select :x") 1))
           ":x");
      Alcotest.(check bool)
        "a value no placeholder takes is refused" true
        (contains
           (db_error
              (Sq.run db
                 (S.find ~params:S.(t2 int int) ~row:S.int "select $1")
                 (1, 2)))
           "2 values");
      Alcotest.(check bool)
        "a placeholder past the values is refused" true
        (contains
           (db_error
              (Sq.run db (S.find ~params:S.int ~row:S.int "select $1 + $2") 1))
           "$2");
      Alcotest.(check bool)
        "two statements in one are refused" true
        (contains
           (db_error
              (Sq.run db (S.exec ~params:S.unit "select 1; select 2") ()))
           "exec_raw"))

(* ------------------------------------------------------------------ *)
(* Scalars *)

let ptime = Alcotest.testable Ptime.pp Ptime.equal
let uuid = Alcotest.testable Uuidm.pp Uuidm.equal

let interval =
  Alcotest.testable
    (fun f (i : S.interval) ->
      Format.fprintf f "%d months %d days %d us" i.months i.days i.microseconds)
    (fun (a : S.interval) b ->
      a.months = b.months && a.days = b.days && a.microseconds = b.microseconds)

(* A value through a STRICT table's column of [column], where SQLite
   refuses one of any other kind, and back. *)
let back db ~column ty v =
  ok (Sq.exec_raw db "drop table if exists v");
  ok (Sq.exec_raw db (Printf.sprintf "create table v (x %s) strict" column));
  ok (Sq.run db (S.exec ~params:ty "insert into v values ($1)") v);
  ok (Sq.run db (S.find ~params:S.unit ~row:ty "select x from v") ())

let round_trips ~column ty testable values =
  on_db [] (fun db ->
      List.iter
        (fun v -> Alcotest.check testable column v (back db ~column ty v))
        values)

let property ~column ty gen eq =
  on_db [] (fun db ->
      QCheck.Test.check_exn
        (QCheck.Test.make ~count:300 ~name:column (QCheck.make gen) (fun v ->
             eq v (back db ~column ty v))))

(* Any instant of years 0 to 9999, to the microsecond. *)
let instant_gen =
  QCheck.Gen.(
    map2
      (fun s us -> after (some (Ptime.of_float_s (Float.of_int s))) us)
      (int_range (-62167219200) 253402300799)
      (int_range 0 999_999))

let test_integers () =
  round_trips ~column:"integer" S.int Alcotest.int
    [ 0; 1; -1; max_int; min_int ];
  round_trips ~column:"integer" S.int64 Alcotest.int64
    [ 0L; Int64.max_int; Int64.min_int ];
  property ~column:"integer" S.int QCheck.Gen.int Int.equal;
  property ~column:"integer" S.int64 QCheck.Gen.int64 Int64.equal

let test_floats () =
  round_trips ~column:"real" S.float (Alcotest.float 0.)
    [
      0.;
      1.5;
      -1e300;
      Float.infinity;
      Float.neg_infinity;
      Float.min_float;
      Float.max_float;
    ];
  property ~column:"real" S.float
    QCheck.Gen.(map (fun f -> if Float.is_nan f then 0. else f) float)
    Float.equal

let test_text_and_bytes () =
  round_trips ~column:"text" S.text Alcotest.string
    [ ""; "plain"; "ünïcödé"; "with\000nul"; "'quoted' \"too\"" ];
  round_trips ~column:"blob" S.bytes Alcotest.string [ ""; "\000\001\255" ];
  property ~column:"text" S.text QCheck.Gen.string String.equal;
  property ~column:"blob" S.bytes QCheck.Gen.string String.equal;
  round_trips ~column:"text" S.json Alcotest.string [ {|{"a":[1,2]}|}; "123" ]

let test_bools () =
  round_trips ~column:"integer" S.bool Alcotest.bool [ true; false ]

let test_times () =
  let at y mo d h mi s us = after (utc y mo d h mi s) us in
  let edges =
    [
      Ptime.epoch;
      at 2026 10 20 9 24 46 621_000;
      at 1 1 1 0 0 0 0;
      at 9999 12 31 23 59 59 999_999;
      at 0 1 1 0 0 0 1;
    ]
  in
  round_trips ~column:"text" S.instant ptime edges;
  round_trips ~column:"text" S.timestamp ptime edges;
  property ~column:"text" S.instant instant_gen Ptime.equal;
  property ~column:"text" S.timestamp instant_gen Ptime.equal;
  round_trips ~column:"text" S.date
    Alcotest.(triple int int int)
    [ (2026, 10, 20); (1, 1, 1); (9999, 12, 31); (2024, 2, 29) ];
  property ~column:"text" S.date
    QCheck.Gen.(map (fun t -> Ptime.to_date t) instant_gen)
    (fun (a, b, c) (x, y, z) -> a = x && b = y && c = z)

let test_intervals () =
  round_trips ~column:"text" S.interval interval
    [
      { months = 0; days = 0; microseconds = 0 };
      { months = 14; days = 3; microseconds = -5 };
      { months = -1; days = -2; microseconds = 1_500_000 };
      { months = max_int; days = min_int; microseconds = min_int };
      { months = min_int; days = max_int; microseconds = max_int };
    ];
  property ~column:"text" S.interval
    QCheck.Gen.(
      map3
        (fun months days microseconds -> { S.months; days; microseconds })
        int int int)
    (fun (a : S.interval) b ->
      a.months = b.months && a.days = b.days && a.microseconds = b.microseconds)

let test_uuids () =
  let v4 = Uuidm.v4_gen (Random.State.make [| 7 |]) in
  round_trips ~column:"text" S.uuid uuid [ Uuidm.nil; Uuidm.max; v4 (); v4 () ]

type colour = Red | Green

let colour_label = function Red -> "red" | Green -> "green"

let test_enums () =
  let colour = S.enum colour_label [ Red; Green ] in
  on_db [] (fun db ->
      Alcotest.(check string)
        "a label" "green"
        (colour_label (back db ~column:"text" colour Green));
      Alcotest.(check bool)
        "a label none of the values has is refused" true
        (contains
           (db_error
              (Sq.run db (S.find ~params:S.unit ~row:colour "select 'blue'") ()))
           "a label none of the values has"))

(* What each scalar is kept as, which a statement in SQL reads: SQLite's
   date and time functions, a JSON array's [json_each]. *)
let test_stored_forms () =
  on_db [] (fun db ->
      let stored ty v =
        ok
          (Sq.run db
             (S.find ~params:ty
                ~row:S.(t2 text text)
                "select typeof($1), $1 || ''")
             v)
      in
      let check name ty v expected =
        Alcotest.(check (pair string string)) name expected (stored ty v)
      in
      let instant = after (utc 2026 10 20 9 24 46) 621_000 in
      check "instant" S.instant instant ("text", "2026-10-20 09:24:46.621000");
      check "timestamp" S.timestamp instant
        ("text", "2026-10-20 09:24:46.621000");
      check "date" S.date (2026, 10, 20) ("text", "2026-10-20");
      check "interval" S.interval
        { months = 14; days = 3; microseconds = -5 }
        ("text", "P14M3DT-0.000005S");
      check "bool" S.bool true ("integer", "1");
      check "uuid" S.uuid
        (some (Uuidm.of_string "7C9E6679-7425-40DE-944B-E07FC1F90AE7"))
        ("text", "7c9e6679-7425-40de-944b-e07fc1f90ae7");
      check "array"
        S.(array (opt text))
        [ Some "a\"b"; None ]
        ("text", {|["a\"b",null]|});
      check "bytes in an array"
        S.(array bytes)
        [ "\000\255" ] ("text", {|["00ff"]|});
      Alcotest.(check bool)
        "an instant compares with datetime('now')" true
        (ok
           (Sq.run db
              (S.find ~params:S.instant ~row:S.bool
                 "select $1 < datetime('now')")
              (utc 2020 1 1 0 0 0))))

(* What SQLite's own functions write, and what another program may have. *)
let test_reads_sqlites_own () =
  on_db [] (fun db ->
      let read ty sql = Sq.run db (S.find ~params:S.unit ~row:ty sql) () in
      Alcotest.check ptime "datetime() in UTC" (utc 2026 10 20 9 24 46)
        (ok (read S.instant "select datetime('2026-10-20 09:24:46')"));
      Alcotest.check ptime "a T and a zone"
        (after (utc 2026 10 20 7 24 46) 500_000)
        (ok (read S.instant "select '2026-10-20T09:24:46.5+02:00'"));
      Alcotest.check ptime "Z" (utc 2026 10 20 9 24 0)
        (ok (read S.instant "select '2026-10-20T09:24Z'"));
      Alcotest.(check bool)
        "now" true
        (Ptime.is_later
           (ok (read S.instant "select strftime('%Y-%m-%d %H:%M:%f', 'now')"))
           ~than:(utc 2026 1 1 0 0 0));
      Alcotest.(check bool)
        "a timestamp with a zone is refused" true
        (contains
           (db_error (read S.timestamp "select '2026-10-20 09:24:46Z'"))
           "Rowtype.instant");
      Alcotest.(check (triple int int int))
        "date()" (2026, 10, 20)
        (ok (read S.date "select date('2026-10-20 23:59:59')"));
      Alcotest.check uuid "a uuid as sixteen bytes"
        (some (Uuidm.of_string "7c9e6679-7425-40de-944b-e07fc1f90ae7"))
        (ok (read S.uuid "select x'7c9e6679742540de944be07fc1f90ae7'"));
      Alcotest.(check (float 0.))
        "a float from an integer" 3.
        (ok (read S.float "select 3")))

(* A value of another kind is refused, by its kind, and the value is in no
   failure's words. *)
let test_refusals () =
  on_db [] (fun db ->
      let read ty sql = Sq.run db (S.find ~params:S.unit ~row:ty sql) () in
      let refused name ty sql words =
        let m = db_error (read ty sql) in
        if not (contains m words) then Alcotest.failf "%s: %S" name m;
        if contains m "hunter2" then Alcotest.failf "%s: the value in %S" name m
      in
      refused "text as an int" S.int "select 'hunter2'"
        "expected INTEGER, got TEXT";
      refused "an int past 63 bits" S.int "select 9223372036854775807"
        "Rowtype.int64";
      refused "a bool of 2" S.bool "select 2" "0 or 1";
      refused "a float past 53 bits" S.float "select 9007199254740993" "53 bits";
      refused "blob as text" S.text "select cast('hunter2' as blob)"
        "expected TEXT, got BLOB";
      refused "text as bytes" S.bytes "select 'hunter2'" "expected BLOB";
      refused "text as an instant" S.instant "select 'hunter2'" "an instant";
      refused "text as a uuid" S.uuid "select 'hunter2'" "a uuid";
      refused "text as an interval" S.interval "select 'hunter2'" "an interval";
      refused "a date out of range" S.date "select '2026-02-30'" "a date";
      refused "NULL" S.text "select null" "NULL";
      Alcotest.(check int)
        "the int64 reads it" 0
        (Int64.compare Int64.max_int
           (ok (read S.int64 "select 9223372036854775807"))))

(* ------------------------------------------------------------------ *)
(* Arrays *)

let test_arrays () =
  on_db [ "create table t (k integer)" ] (fun db ->
      let back ty v =
        ok (Sq.run db (S.find ~params:ty ~row:ty "select $1") v)
      in
      Alcotest.(check (list int))
        "ints" [ 1; -2; max_int ]
        (back S.(array int) [ 1; -2; max_int ]);
      Alcotest.(check (list int)) "empty" [] (back S.(array int) []);
      Alcotest.(check (list (option string)))
        "text with NULLs and escapes"
        [ Some "a\"b\\c\n\001ü"; None; Some "" ]
        (back S.(array (opt text)) [ Some "a\"b\\c\n\001ü"; None; Some "" ]);
      Alcotest.(check (list string))
        "bytes" [ "\000\255"; "" ]
        (back S.(array bytes) [ "\000\255"; "" ]);
      Alcotest.(check (list (float 0.)))
        "floats"
        [ 0.1; 3.; Float.infinity; -1e300 ]
        (back S.(array float) [ 0.1; 3.; Float.infinity; -1e300 ]);
      Alcotest.(check (list bool))
        "bools" [ true; false ]
        (back S.(array bool) [ true; false ]);
      Alcotest.(check (list ptime))
        "instants" [ Ptime.epoch ]
        (back S.(array instant) [ Ptime.epoch ]);
      ok
        (Sq.run db
           (S.exec
              ~params:S.(array int)
              "insert into t select value from json_each($1)")
           [ 3; 1; 2 ]);
      Alcotest.(check (list int))
        "json_each writes it, json_group_array reads it" [ 1; 2; 3 ]
        (ok
           (Sq.run db
              (S.find ~params:S.unit
                 ~row:S.(array int)
                 "select json_group_array(k) from (select k from t order by k)")
              ()));
      let read sql =
        Sq.run db (S.find ~params:S.unit ~row:S.(array int) sql) ()
      in
      Alcotest.(check bool)
        "two dimensions are refused" true
        (contains
           (db_error (read "select '[[1],[2]]'"))
           "more than one dimension");
      Alcotest.(check bool)
        "an object is refused" true
        (contains (db_error (read {|select '[{"a":1}]'|})) "object");
      Alcotest.(check bool)
        "text that is no array is refused" true
        (contains (db_error (read "select 'hunter2'")) "not an array");
      Alcotest.(check bool)
        "an element of another kind names the element" true
        (contains (db_error (read {|select '[1,"x"]'|})) "element 2"))

(* ------------------------------------------------------------------ *)
(* Failures *)

let test_conflicts () =
  on_db
    [
      "create table p (id integer primary key, email text unique, n integer \
       not null check (n > 0))";
      "create table c (p integer references p (id))";
      "insert into p values (1, 'a@b', 1)";
    ] (fun db ->
      let run sql = Sq.run db (S.exec ~params:S.unit sql) () in
      let conflict name expected r =
        match r with
        | Error (`Conflict n) when Option.equal String.equal n expected -> ()
        | Error e -> Alcotest.failf "%s: %s" name (Sq.error_to_string e)
        | Ok () -> Alcotest.failf "%s: written" name
      in
      conflict "unique" (Some "p.email")
        (run "insert into p values (2, 'a@b', 1)");
      conflict "primary key" (Some "p.id")
        (run "insert into p values (1, 'c@d', 1)");
      conflict "foreign key" None (run "insert into c values (9)");
      Alcotest.(check bool)
        "not null is the program's" true
        (contains
           (db_error (run "insert into p values (3, 'e@f', null)"))
           "NOT NULL");
      Alcotest.(check bool)
        "check is the program's" true
        (contains (db_error (run "insert into p values (3, 'e@f', 0)")) "CHECK");
      Alcotest.(check bool)
        "SQL SQLite refuses" true
        (contains (db_error (run "selec 1")) "syntax error"))

let test_busy () =
  on_two
    ~timeout:Mtime.Span.(10 * ms)
    [ "create table t (k integer)" ]
    (fun a b ->
      ok (Sq.exec_raw a "begin immediate");
      (match Sq.run b (S.exec ~params:S.unit "insert into t values (1)") () with
      | Error (`Not_serializable _) -> ()
      | Error e -> Alcotest.failf "unexpected error: %s" (Sq.error_to_string e)
      | Ok () -> Alcotest.fail "written under another's lock");
      ok (Sq.exec_raw a "rollback");
      ok (Sq.run b (S.exec ~params:S.unit "insert into t values (1)") ()))

(* A statement waiting on a lock leaves the domain to its other fibers: the
   fiber that lets the lock go runs while the write waits, or the write
   waits out its five seconds and fails. *)
let test_waits_off_the_domain () =
  on_two [ "create table t (k integer)" ] (fun a b ->
      ok (Sq.exec_raw a "begin immediate");
      Eio.Fiber.both
        (fun () ->
          ok (Sq.run b (S.exec ~params:S.unit "insert into t values (1)") ()))
        (fun () -> ok (Sq.exec_raw a "rollback"));
      Alcotest.(check int) "written once the lock went" 1 (count a "t"))

let test_closed () =
  with_eio @@ fun ~sw ~mono_clock ->
  let db = ok (Sq.connect ~sw ~mono_clock ":memory:") in
  Sq.close db;
  Sq.close db;
  (match Sq.run db (S.find ~params:S.unit ~row:S.int "select 1") () with
  | Error (`Connection `Closed) -> ()
  | _ -> Alcotest.fail "a closed connection ran a statement");
  match Sq.connect ~sw ~mono_clock "/no/such/directory/x.db" with
  | Error (`Unsent _) -> ()
  | Error e -> Alcotest.failf "unexpected error: %s" (Sq.error_to_string e)
  | Ok _ -> Alcotest.fail "opened a file in no directory"

let test_foreign_keys_on () =
  on_db [] (fun db ->
      Alcotest.(check int)
        "enforced" 1
        (ok
           (Sq.run db
              (S.find ~params:S.unit ~row:S.int "pragma foreign_keys")
              ())))

(* A fiber cancelled while it reads ends its statement: a read left open
   would hold the file's lock and keep the other connection from writing. *)
let test_cancelled_fold () =
  on_two
    ~timeout:Mtime.Span.(10 * ms)
    [
      "create table t (k integer)";
      "with recursive n(i) as (select 1 union all select i + 1 from n where i \
       < 1000) insert into t select i from n";
    ]
    (fun a b ->
      let never, _ = Eio.Promise.create () in
      let reading, read = Eio.Promise.create () in
      Eio.Fiber.first
        (fun () ->
          ignore
            (Sq.fold a (S.list ~params:S.unit ~row:S.int "select k from t")
               () ~init:() (fun () _ ->
                 if not (Eio.Promise.is_resolved reading) then
                   Eio.Promise.resolve read ();
                 Eio.Promise.await never)
              : (unit, Sq.error) result))
        (fun () -> Eio.Promise.await reading);
      ok (Sq.run b (S.exec ~params:S.unit "insert into t values (0)") ());
      Alcotest.(check int) "the connection goes on" 1001 (count a "t"))

(* What is logged and observed of a statement: its text, never a value. *)
let test_no_value_logged () =
  let lines = ref [] in
  let reporter =
    {
      Logs.report =
        (fun src _ ~over k msgf ->
          msgf (fun ?header:_ ?tags:_ fmt ->
              Format.kasprintf
                (fun line ->
                  if String.equal (Logs.Src.name src) "rowtype-sqlite" then
                    lines := line :: !lines;
                  over ();
                  k ())
                fmt));
    }
  in
  let seen = ref [] in
  let observe =
    {
      Sq.around =
        (fun sql run ->
          seen := sql :: !seen;
          run ());
    }
  in
  Logs.set_reporter reporter;
  Logs.set_level (Some Logs.Debug);
  Fun.protect
    ~finally:(fun () -> Logs.set_level None)
    (fun () ->
      with_eio @@ fun ~sw ~mono_clock ->
      let db = ok (Sq.connect ~sw ~mono_clock ~observe ":memory:") in
      ok (Sq.exec_raw db "create table t (v text)");
      ok
        (Sq.Transaction.within db (fun db ->
             Sq.run db
               (S.exec ~params:S.text "insert into t values ($1)")
               "hunter2"));
      Alcotest.(check bool)
        "the statement is logged" true
        (List.exists (fun l -> contains l "insert into t values ($1)") !lines);
      Alcotest.(check bool)
        "the value is not" false
        (List.exists (fun l -> contains l "hunter2") !lines);
      Alcotest.(check bool)
        "the observer sees begin, the statement and commit" true
        (List.exists (String.equal "insert into t values ($1)") !seen
        && List.exists (fun s -> contains s "begin") !seen
        && List.exists (String.equal "commit") !seen))

(* ------------------------------------------------------------------ *)
(* Transactions *)

let insert = S.exec ~params:S.int "insert into t values ($1)"

let test_commit_and_rollback () =
  on_db [ "create table t (k integer primary key)" ] (fun db ->
      Alcotest.(check int)
        "Ok commits" 7
        (ok
           (Sq.Transaction.within db (fun db ->
                Result.map (fun () -> 7) (Sq.run db insert 1))));
      (match
         Sq.Transaction.within db (fun db ->
             match Sq.run db insert 2 with
             | Ok () -> Error `Refused
             | Error e -> Error e)
       with
      | Error `Refused -> ()
      | _ -> Alcotest.fail "the refusal was not the answer");
      Alcotest.(check int) "Error rolls back" 1 (count db "t");
      (match
         Sq.Transaction.within db
           ~keep:(function `Refused -> true | _ -> false)
           (fun db ->
             match Sq.run db insert 3 with
             | Ok () -> Error `Refused
             | Error e -> Error e)
       with
      | Error `Refused -> ()
      | _ -> Alcotest.fail "the kept refusal was not the answer");
      Alcotest.(check int) "keep keeps" 2 (count db "t");
      (* A failed statement undoes itself alone, as SQLite does. *)
      ok
        (Sq.Transaction.within db (fun db ->
             (match Sq.run db insert 1 with
             | Error (`Conflict _) -> ()
             | _ -> Alcotest.fail "no conflict");
             Sq.run db insert 4));
      Alcotest.(check int) "the rest commits" 3 (count db "t"))

let test_nested () =
  on_db [ "create table t (k integer primary key)" ] (fun db ->
      ok
        (Sq.Transaction.within db (fun db ->
             let ( let* ) = Result.bind in
             let* () = Sq.run db insert 1 in
             (match Sq.Transaction.within db (fun db -> Sq.run db insert 2) with
             | Error (`Not_committed _) -> ()
             | _ -> Alcotest.fail "a transaction began inside another");
             Sq.run db insert 3));
      Alcotest.(check int) "the outer one went on" 2 (count db "t"))

let test_rolled_back_by_sqlite () =
  on_db [ "create table t (k integer primary key)" ] (fun db ->
      match
        Sq.Transaction.within db (fun db ->
            let ( let* ) = Result.bind in
            let* () = Sq.run db insert 1 in
            Sq.exec_raw db "rollback")
      with
      | Error (`Not_committed _) -> ()
      | _ -> Alcotest.fail "a transaction with nothing to commit committed")

let test_deferred_conflict () =
  on_db
    [
      "create table p (id integer primary key)";
      "create table c (p integer references p (id) deferrable initially \
       deferred)";
    ] (fun db ->
      (match
         Sq.Transaction.within db (fun db ->
             Sq.run db (S.exec ~params:S.unit "insert into c values (9)") ())
       with
      | Error (`Conflict None) -> ()
      | Error e -> Alcotest.failf "unexpected error: %s" (Sq.error_to_string e)
      | Ok () -> Alcotest.fail "committed an orphan");
      Alcotest.(check int) "rolled back" 0 (count db "c");
      ok (Sq.Transaction.within db (fun _ -> Ok ())))

(* COMMIT needs every reader gone outside WAL mode: one held open is a lock
   COMMIT cannot take, and the transaction is rolled back. *)
let test_commit_busy () =
  on_two
    ~timeout:Mtime.Span.(10 * ms)
    [ "create table t (k integer)" ]
    (fun a b ->
      ok (Sq.exec_raw b "begin; select * from t");
      (match Sq.Transaction.within a (fun a -> Sq.run a insert 1) with
      | Error (`Not_serializable _) -> ()
      | Error e -> Alcotest.failf "unexpected error: %s" (Sq.error_to_string e)
      | Ok () -> Alcotest.fail "committed under a reader");
      ok (Sq.exec_raw b "rollback");
      Alcotest.(check int) "rolled back" 0 (count a "t");
      ok (Sq.Transaction.within a (fun a -> Sq.run a insert 1)))

let test_retries () =
  on_db [ "create table t (k integer)" ] (fun db ->
      let attempts = ref 0 in
      let work db =
        incr attempts;
        let ( let* ) = Result.bind in
        let* () = Sq.run db insert !attempts in
        if !attempts < 3 then Error (`Not_serializable "again") else Ok ()
      in
      ok (Sq.Transaction.within ~retries:5 db work);
      Alcotest.(check int) "three attempts" 3 !attempts;
      Alcotest.(check int) "the last one's row alone" 1 (count db "t");
      attempts := 0;
      (match Sq.Transaction.within ~retries:1 db work with
      | Error (`Not_serializable _) -> ()
      | _ -> Alcotest.fail "the last answer did not stand");
      Alcotest.(check int) "two attempts" 2 !attempts)

let test_behaviours () =
  on_two
    ~timeout:Mtime.Span.(10 * ms)
    [ "create table t (k integer)" ]
    (fun a b ->
      List.iter
        (fun behaviour ->
          ok (Sq.Transaction.within ~behaviour a (fun a -> Sq.run a insert 1)))
        Sq.Transaction.[ Deferred; Immediate; Exclusive ];
      Alcotest.(check int) "each committed" 3 (count a "t");
      ok (Sq.exec_raw b "begin immediate");
      (match Sq.Transaction.within ~behaviour:Immediate a (fun _ -> Ok ()) with
      | Error (`Not_serializable _) -> ()
      | _ -> Alcotest.fail "began immediate under another's lock");
      ok (Sq.exec_raw b "rollback"))

exception Raised

let test_raise_and_cancel () =
  on_db [ "create table t (k integer)" ] (fun db ->
      (match
         Sq.Transaction.within db (fun db ->
             ignore (Sq.run db insert 1 : (unit, Sq.error) result);
             raise Raised)
       with
      | _ -> Alcotest.fail "the raise did not pass"
      | exception Raised -> ());
      Alcotest.(check int) "a raise rolls back" 0 (count db "t");
      let never, _ = Eio.Promise.create () in
      let written, write = Eio.Promise.create () in
      Eio.Fiber.first
        (fun () ->
          ignore
            (Sq.Transaction.within db (fun db ->
                 ok (Sq.run db insert 1);
                 Eio.Promise.resolve write ();
                 Eio.Promise.await never)
              : (unit, Sq.error) result))
        (fun () -> Eio.Promise.await written);
      Alcotest.(check int) "a cancellation rolls back" 0 (count db "t");
      ok (Sq.Transaction.within db (fun db -> Sq.run db insert 1));
      Alcotest.(check int) "and the connection goes on" 1 (count db "t"))

(* ------------------------------------------------------------------ *)
(* A load *)

let numbers n = Seq.init n (fun i -> (i + 1, Printf.sprintf "row %d" (i + 1)))

let test_copy_in () =
  on_db [ "create table t (k integer primary key, v text, w text)" ] (fun db ->
      Alcotest.(check int)
        "every row, past many chunks" 10_000
        (ok
           (Sq.copy_in db ~table:"t" ~columns:[ "k"; "v" ]
              S.(t2 int text)
              (numbers 10_000)));
      Alcotest.(check int) "written" 10_000 (count db "t");
      Alcotest.(check (option string))
        "in its column" (Some "row 7")
        (ok
           (Sq.run db
              (S.find ~params:S.int
                 ~row:S.(opt text)
                 "select v from t where k = $1")
              7));
      ok (Sq.exec_raw db "delete from t");
      Alcotest.(check int)
        "every column in table order where none are named" 1
        (ok
           (Sq.copy_in db ~table:"t" ~columns:[]
              S.(t3 int text (opt text))
              (List.to_seq [ (1, "a", None) ])));
      Alcotest.(check int)
        "the database in [schema]" 1
        (ok
           (Sq.copy_in db ~schema:"main" ~table:"t" ~columns:[ "k" ] S.int
              (List.to_seq [ 2 ])));
      Alcotest.(check bool)
        "columns that are not the row are refused" true
        (contains
           (db_error
              (Sq.copy_in db ~table:"t" ~columns:[ "k" ]
                 S.(t2 int text)
                 (List.to_seq [ (3, "x") ])))
           "2"))

exception From_the_rows

let test_copy_in_failures () =
  on_db [ "create table t (k integer primary key, v text not null)" ] (fun db ->
      let load rows =
        Sq.copy_in db ~table:"t" ~columns:[ "k"; "v" ] S.(t2 int text) rows
      in
      (match load (List.to_seq [ (1, "a"); (2, "b"); (1, "c") ]) with
      | Error (`Conflict (Some "t.k")) -> ()
      | Error e -> Alcotest.failf "unexpected error: %s" (Sq.error_to_string e)
      | Ok _ -> Alcotest.fail "a repeated key was written");
      Alcotest.(check int) "a constraint writes nothing" 0 (count db "t");
      (match
         load (Seq.append (numbers 100) (fun () -> raise From_the_rows))
       with
      | _ -> Alcotest.fail "the raise did not pass"
      | exception From_the_rows -> ());
      Alcotest.(check int)
        "a raise from the rows writes nothing" 0 (count db "t");
      (match
         Sq.Transaction.within db (fun _ ->
             match load (numbers 5) with
             | Ok _ -> Error `Refused
             | Error e -> Error e)
       with
      | Error `Refused -> ()
      | _ -> Alcotest.fail "the load was not refused");
      Alcotest.(check int)
        "a load rolls back with its transaction" 0 (count db "t"))

(* ------------------------------------------------------------------ *)
(* A pool *)

let on_pool ?size ?wait setup f =
  with_file @@ fun file ->
  with_eio @@ fun ~sw ~mono_clock ->
  let pool = ok (Sq.Pool.create ~sw ~mono_clock ?size ?wait file) in
  ok
    (Result.join
       (Sq.Pool.use pool (fun db ->
            List.fold_left
              (fun acc s -> Result.bind acc (fun () -> Sq.exec_raw db s))
              (Ok ()) setup)));
  f pool

let test_pool () =
  on_pool ~size:2 [ "create table t (k integer)" ] (fun pool ->
      Alcotest.(check int) "made up front" 2 (Sq.Pool.stats pool).idle;
      ok (Result.join (Sq.Pool.use pool (fun db -> Sq.run db insert 1)));
      Alcotest.(check int)
        "one file, every connection" 1
        (ok (Sq.Pool.use pool (fun db -> count db "t")));
      Alcotest.(check int) "given back" 2 (Sq.Pool.stats pool).idle;
      (match Sq.Pool.use pool (fun _ -> raise Stop) with
      | _ -> Alcotest.fail "the raise did not pass"
      | exception Stop -> ());
      Alcotest.(check int)
        "given back after a raise" 2 (Sq.Pool.stats pool).idle)

(* Every connection lent, the next borrow waits its wait and is [`Busy];
   a connection given back while one waits is lent to it. *)
let test_pool_busy () =
  on_pool ~size:1
    ~wait:Mtime.Span.(10 * ms)
    []
    (fun pool ->
      let release, released = Eio.Promise.create () in
      Eio.Fiber.both
        (fun () -> ok (Sq.Pool.use pool (fun _ -> Eio.Promise.await release)))
        (fun () ->
          (match Sq.Pool.use pool (fun _ -> ()) with
          | Error (`Pool `Busy) -> ()
          | _ -> Alcotest.fail "a borrow did not wait out its wait");
          Alcotest.(check int) "lent" 0 (Sq.Pool.stats pool).idle;
          Eio.Fiber.both
            (fun () ->
              ok (Sq.Pool.use ~wait:Mtime.Span.(60 * s) pool (fun _ -> ())))
            (fun () -> Eio.Promise.resolve released ())))

(* A transaction a borrower left open is not the next borrower's. *)
let test_pool_clean () =
  on_pool ~size:1 [ "create table t (k integer)" ] (fun pool ->
      ok
        (Result.join
           (Sq.Pool.use pool (fun db ->
                Result.bind (Sq.exec_raw db "begin") (fun () ->
                    Sq.run db insert 1))));
      ok
        (Result.join
           (Sq.Pool.use pool (fun db ->
                Sq.Transaction.within db (fun db -> Sq.run db insert 2))));
      Alcotest.(check int)
        "the left transaction rolled back" 1
        (ok (Sq.Pool.use pool (fun db -> count db "t"))))

let test_pool_closed () =
  on_pool ~size:1 [] (fun pool ->
      let release, released = Eio.Promise.create () in
      Eio.Fiber.both
        (fun () -> ok (Sq.Pool.use pool (fun _ -> Eio.Promise.await release)))
        (fun () ->
          Eio.Fiber.both
            (fun () ->
              match
                Sq.Pool.use ~wait:Mtime.Span.(60 * s) pool (fun _ -> ())
              with
              | Error (`Pool `Closed) -> ()
              | _ -> Alcotest.fail "a waiting borrow did not see the close")
            (fun () -> Sq.Pool.close pool);
          Eio.Promise.resolve released ());
      match Sq.Pool.use pool (fun _ -> ()) with
      | Error (`Pool `Closed) -> ()
      | _ -> Alcotest.fail "a closed pool lent a connection")

let test_pool_memory () =
  with_eio @@ fun ~sw ~mono_clock ->
  match Sq.Pool.create ~sw ~mono_clock ":memory:" with
  | Error (`Db m) when contains m "file" -> ()
  | Error e -> Alcotest.failf "unexpected error: %s" (Sq.error_to_string e)
  | Ok _ -> Alcotest.fail "a pool of databases of one connection each"

(* ------------------------------------------------------------------ *)
(* Statements checked *)

let disagreements = function
  | Error (`Disagreements problems) -> problems
  | Ok () -> []
  | Error e -> Alcotest.failf "unexpected error: %s" (Sq.error_to_string e)

let test_verify () =
  on_db
    [
      "create table t (k integer primary key, name text not null, price real, \
       body blob, at text, flags)";
    ] (fun db ->
      Alcotest.(check (list string))
        "statements that agree" []
        (disagreements
           (Sq.verify db
              [
                S.Any
                  (S.find_opt ~params:S.int
                     ~row:S.(t4 int text (opt float) (opt bytes))
                     "select k, name, price, body from t where k = $1");
                S.Any
                  (S.exec
                     ~params:S.(t3 int text instant)
                     "insert into t (k, name, at) values ($1, $2, $3)");
                S.Any
                  (S.list ~params:S.unit
                     ~row:S.(t2 int64 (opt json))
                     "select count(*), flags from t");
                S.Any
                  (S.find ~params:S.(t2 int int) ~row:S.int "select ?2 + $1");
              ]));
      let found statement words =
        match disagreements (Sq.verify db [ S.Any statement ]) with
        | [ p ] ->
            if not (contains p words) then Alcotest.failf "%S: %s" words p
        | ps -> Alcotest.failf "%d problems for %S" (List.length ps) words
      in
      found
        (S.find ~params:S.unit ~row:S.int "select name from t")
        "column 1 is TEXT, which int does not read";
      found
        (S.find ~params:S.unit ~row:S.bytes "select k from t")
        "column 1 is INTEGER, which bytes does not read";
      found
        (S.find ~params:S.unit ~row:S.(t2 int text) "select k from t")
        "2 columns declared, and the database has 1";
      found
        (S.exec ~params:S.(t2 int int) "delete from t where k = $1")
        "2 parameters declared, and the database has 1";
      found (S.exec ~params:S.int "delete from t where k = :k") ":k";
      found
        (S.find ~params:S.unit ~row:S.int "select nope from t")
        "the database refuses it";
      Alcotest.(check bool)
        "each problem names its statement" true
        (List.for_all
           (fun p -> String.starts_with ~prefix:"select name from t" p)
           (disagreements
              (Sq.verify db
                 [
                   S.Any (S.find ~params:S.unit ~row:S.int "select name from t");
                 ]))))

(* ------------------------------------------------------------------ *)
(* WAL, and the pool under it *)

let wal setup f =
  with_file @@ fun file ->
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let mono_clock = Eio.Stdenv.mono_clock env in
  let first = ok (Sq.connect ~sw ~mono_clock file) in
  ok (Sq.exec_raw first "pragma journal_mode = wal");
  List.iter (fun s -> ok (Sq.exec_raw first s)) setup;
  Sq.close first;
  f env ~sw ~mono_clock file

(* A reader's transaction keeps what it saw while a writer commits, which
   outside WAL mode the writer could not do while the reader read. *)
let test_wal_readers_beside_a_writer () =
  wal [ "create table t (k integer)" ] @@ fun _ ~sw ~mono_clock file ->
  let reader = ok (Sq.connect ~sw ~mono_clock file)
  and writer =
    ok (Sq.connect ~sw ~mono_clock ~timeout:Mtime.Span.(10 * ms) file)
  in
  ok (Sq.exec_raw reader "begin; select count(*) from t");
  ok (Sq.run writer insert 1);
  Alcotest.(check int) "the reader's snapshot" 0 (count reader "t");
  ok (Sq.exec_raw reader "commit");
  Alcotest.(check int) "and after it, the write" 1 (count reader "t")

(* A deferred transaction that read before another connection wrote cannot
   then write, however long it waits: it is told [`Not_serializable], and
   one begun [Immediate] waits its turn instead. *)
let test_wal_a_deferred_write_after_a_read () =
  wal [ "create table t (k integer)" ] @@ fun _ ~sw ~mono_clock file ->
  let a = ok (Sq.connect ~sw ~mono_clock file)
  and b = ok (Sq.connect ~sw ~mono_clock file) in
  let read_then_write behaviour =
    Sq.Transaction.within ~behaviour a (fun a ->
        let ( let* ) = Result.bind in
        let* (_ : int) =
          Sq.run a
            (S.find ~params:S.unit ~row:S.int "select count(*) from t")
            ()
        in
        let* () = Sq.run b insert 1 in
        Sq.run a insert 2)
  in
  (match read_then_write Deferred with
  | Error (`Not_serializable _) -> ()
  | Ok () -> Alcotest.fail "wrote over another's write"
  | Error e -> Alcotest.failf "unexpected error: %s" (Sq.error_to_string e));
  Alcotest.(check int) "b's write alone" 1 (count a "t")

let test_pool_on_domains () =
  wal [ "create table t (k integer)" ] @@ fun env ~sw ~mono_clock file ->
  let pool = ok (Sq.Pool.create ~sw ~mono_clock ~size:4 file) in
  let writes = 200 in
  let on_a_domain () =
    Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
        for k = 1 to writes do
          ok (Result.join (Sq.Pool.use pool (fun db -> Sq.run db insert k)))
        done)
  in
  Eio.Fiber.all [ on_a_domain; on_a_domain; on_a_domain ];
  Alcotest.(check int)
    "every write, from every domain" (3 * writes)
    (ok (Sq.Pool.use pool (fun db -> count db "t")));
  Alcotest.(check int) "every connection back" 4 (Sq.Pool.stats pool).idle

(* A connection its borrower closed is made again before it is lent. *)
let test_pool_remakes_a_closed_connection () =
  on_pool ~size:1 [ "create table t (k integer)" ] (fun pool ->
      ok (Sq.Pool.use pool Sq.close);
      ok (Result.join (Sq.Pool.use pool (fun db -> Sq.run db insert 1)));
      Alcotest.(check int)
        "lent working" 1
        (ok (Sq.Pool.use pool (fun db -> count db "t"))))

(* A borrow cancelled while it waits leaves nothing behind it. *)
let test_pool_cancelled_borrow () =
  on_pool ~size:1 [] (fun pool ->
      let release, released = Eio.Promise.create () in
      Eio.Fiber.both
        (fun () -> ok (Sq.Pool.use pool (fun _ -> Eio.Promise.await release)))
        (fun () ->
          Eio.Fiber.first
            (fun () ->
              ignore
                (Sq.Pool.use ~wait:Mtime.Span.(60 * s) pool (fun _ -> ())
                  : (unit, Sq.error) result))
            (fun () -> ());
          Alcotest.(check int)
            "no borrow waiting" 0 (Sq.Pool.stats pool).waiting;
          Eio.Promise.resolve released ());
      Alcotest.(check int) "the connection back" 1 (Sq.Pool.stats pool).idle;
      ok (Sq.Pool.use pool (fun _ -> ())))

(* ------------------------------------------------------------------ *)
(* Reads of any text *)

(* Every scalar read from text of any bytes answers a value or a refusal,
   and raises nothing: each text form is read by a parser that must hold
   on whatever another program stored. *)
let test_any_text_is_read_or_refused () =
  on_db [] (fun db ->
      let reads (type a) (ty : a S.ty) text =
        match Sq.run db (S.find ~params:S.text ~row:ty "select $1") text with
        | Ok _ | Error (`Db _) -> true
        | Error _ -> false
      in
      let near =
        QCheck.Gen.(
          oneof
            [
              string;
              string_of
                (oneof_list
                   [
                     '0';
                     '1';
                     '9';
                     '-';
                     ':';
                     ' ';
                     'T';
                     'Z';
                     '+';
                     '.';
                     'P';
                     'M';
                     'D';
                     'S';
                   ]);
              map (fun s -> "[" ^ s ^ "]") string;
              string_of
                (oneof_list
                   [
                     '[';
                     ']';
                     ',';
                     '"';
                     '\\';
                     'u';
                     'd';
                     '8';
                     '0';
                     'n';
                     'l';
                     't';
                     'r';
                     'e';
                     '-';
                     '1';
                     '.';
                   ]);
            ])
      in
      QCheck.Test.check_exn
        (QCheck.Test.make ~count:2000 ~name:"any text" (QCheck.make near)
           (fun text ->
             reads S.instant text && reads S.timestamp text && reads S.date text
             && reads S.interval text && reads S.uuid text
             && reads S.(array (opt text)) text
             && reads S.(array bytes) text
             && reads S.(array float) text)))

(* What SQLite's own date and time functions read, at the edges of a zone
   and a second. *)
let test_times_at_their_edges () =
  on_db [] (fun db ->
      let read ty text =
        Sq.run db (S.find ~params:S.text ~row:ty "select $1") text
      in
      Alcotest.check ptime "fourteen hours ahead" (utc 2026 10 19 19 0 0)
        (ok (read S.instant "2026-10-20 09:00:00+14:00"));
      Alcotest.check ptime "twelve behind" (utc 2026 10 20 21 0 0)
        (ok (read S.instant "2026-10-20T09:00-12:00"));
      Alcotest.check ptime "a fraction past the microsecond"
        (after (utc 2026 10 20 9 0 0) 123_456)
        (ok (read S.instant "2026-10-20 09:00:00.123456789"));
      Alcotest.check ptime "a leap second, the next one"
        (utc 2016 12 31 23 59 60)
        (ok (read S.instant "2016-12-31 23:59:60"));
      List.iter
        (fun text ->
          match read S.instant text with
          | Error (`Db _) -> ()
          | Ok _ -> Alcotest.failf "%S was read" text
          | Error e -> Alcotest.failf "%S: %s" text (Sq.error_to_string e))
        [
          "2026-10-20t09:00:00";
          "2026-10-20 09:00:00z";
          "2026-10-20 24:00:00";
          "2026-10-20 09:00:00.";
          "2026-10-20 09:00:00+2:00";
          "+2026-10-20 09:00:00";
          " 2026-10-20 09:00:00";
        ])

let test_nan_is_null () =
  on_db [ "create table t (x real)" ] (fun db ->
      ok
        (Sq.run db
           (S.exec ~params:S.float "insert into t values ($1)")
           Float.nan);
      Alcotest.(check (option (float 0.)))
        "kept as NULL" None
        (ok
           (Sq.run db
              (S.find ~params:S.unit ~row:S.(opt float) "select x from t")
              ())))

(* A load cancelled part-way writes nothing, and the connection goes on. *)
let test_copy_in_cancelled () =
  on_db [ "create table t (k integer, v text)" ] (fun db ->
      let never, _ = Eio.Promise.create () in
      let loading, loaded = Eio.Promise.create () in
      let rows =
        Seq.append (numbers 500) (fun () ->
            Eio.Promise.resolve loaded ();
            Eio.Promise.await never)
      in
      Eio.Fiber.first
        (fun () ->
          ignore
            (Sq.copy_in db ~table:"t" ~columns:[ "k"; "v" ] S.(t2 int text) rows
              : (int, Sq.error) result))
        (fun () -> Eio.Promise.await loading);
      Alcotest.(check int) "nothing written" 0 (count db "t");
      ignore
        (ok
           (Sq.copy_in db ~table:"t" ~columns:[ "k"; "v" ]
              S.(t2 int text)
              (numbers 3))
          : int);
      Alcotest.(check int) "and it goes on" 3 (count db "t"))

(* Outside WAL mode an exclusive transaction keeps every other connection
   from reading. *)
let test_exclusive_keeps_readers_out () =
  on_two
    ~timeout:Mtime.Span.(10 * ms)
    [ "create table t (k integer)" ]
    (fun a b ->
      let release, released = Eio.Promise.create () in
      Eio.Fiber.both
        (fun () ->
          ok
            (Sq.Transaction.within ~behaviour:Exclusive a (fun a ->
                 Eio.Promise.await release;
                 Sq.run a insert 1)))
        (fun () ->
          (match
             Sq.run b
               (S.find ~params:S.unit ~row:S.int "select count(*) from t")
               ()
           with
          | Error (`Not_serializable _) -> ()
          | _ -> Alcotest.fail "read under an exclusive transaction");
          Eio.Promise.resolve released ());
      Alcotest.(check int) "then readable" 1 (count b "t"))

(* A refusal that keeps its write, whose COMMIT a deferred foreign key then
   refuses, keeps nothing and says so. *)
let test_keep_when_commit_fails () =
  on_db
    [
      "create table p (id integer primary key)";
      "create table c (p integer references p (id) deferrable initially \
       deferred)";
    ] (fun db ->
      (match
         Sq.Transaction.within db
           ~keep:(function `Refused -> true | _ -> false)
           (fun db ->
             match
               Sq.run db (S.exec ~params:S.unit "insert into c values (9)") ()
             with
             | Ok () -> Error `Refused
             | Error e -> Error e)
       with
      | Error (`Conflict None) -> ()
      | Error e ->
          Alcotest.failf "unexpected: %s"
            (Sq.error_to_string
               (match e with `Refused -> `Db "refused" | #Sq.error as e -> e))
      | Ok () -> Alcotest.fail "committed an orphan");
      Alcotest.(check int) "nothing kept" 0 (count db "c"))

(* Each branch of the check: a column's declared type against what reads
   it, an array's column, a uuid's blob, a NUMERIC column that keeps text
   that looks like a number as a number, and a table in an attached
   database. *)
let test_verify_each_branch () =
  on_db
    [
      "create table t (tags text, n integer, id blob, doc json, at timestamp, \
       r real, f float)";
      "attach ':memory:' as aux";
      "create table aux.u (name text)";
    ] (fun db ->
      let agree statement =
        Alcotest.(check (list string))
          "agrees" []
          (disagreements (Sq.verify db [ S.Any statement ]))
      in
      let found statement words =
        match disagreements (Sq.verify db [ S.Any statement ]) with
        | [ p ] ->
            if not (contains p words) then Alcotest.failf "%S: %s" words p
        | ps -> Alcotest.failf "%d problems for %S" (List.length ps) words
      in
      let row ty sql = S.find ~params:S.unit ~row:ty sql in
      agree (row S.(array int) "select tags from t");
      found (row S.(array int) "select n from t") "an array";
      agree (row S.uuid "select id from t");
      agree (row S.bool "select n from t");
      agree (row S.float "select n from t");
      agree (row S.float "select r from t");
      agree (row S.float "select f from t");
      agree (row S.instant "select at from t");
      found (row S.json "select doc from t") "JSON";
      found (row S.text "select r from t") "REAL";
      agree (row S.text "select name from aux.u");
      found (row S.int "select name from aux.u") "TEXT")

let () =
  Alcotest.run "rowtype-sqlite"
    [
      ( "statements",
        [
          Alcotest.test_case "a row of four round-trips" `Quick
            test_roundtrip_t4;
          Alcotest.test_case "find and find_opt count" `Quick test_find;
          Alcotest.test_case "exec_count" `Quick test_exec_count;
          Alcotest.test_case "a fold past one chunk" `Quick test_fold_many;
          Alcotest.test_case "a raise from a fold" `Quick test_fold_raise;
          Alcotest.test_case "run_many is all or nothing" `Quick
            test_run_many_all_or_nothing;
          Alcotest.test_case "run_many inside a transaction" `Quick
            test_run_many_in_transaction;
          Alcotest.test_case "placeholders" `Quick test_placeholders;
        ] );
      ( "scalars",
        [
          Alcotest.test_case "integers" `Quick test_integers;
          Alcotest.test_case "floats" `Quick test_floats;
          Alcotest.test_case "text, bytes and json" `Quick test_text_and_bytes;
          Alcotest.test_case "bools" `Quick test_bools;
          Alcotest.test_case "instants, timestamps and dates" `Quick test_times;
          Alcotest.test_case "intervals" `Quick test_intervals;
          Alcotest.test_case "uuids" `Quick test_uuids;
          Alcotest.test_case "enums" `Quick test_enums;
          Alcotest.test_case "what each is kept as" `Quick test_stored_forms;
          Alcotest.test_case "what SQLite writes" `Quick test_reads_sqlites_own;
          Alcotest.test_case "another kind is refused" `Quick test_refusals;
          Alcotest.test_case "arrays" `Quick test_arrays;
        ] );
      ( "failures",
        [
          Alcotest.test_case "conflicts" `Quick test_conflicts;
          Alcotest.test_case "a lock held" `Quick test_busy;
          Alcotest.test_case "a wait leaves the domain" `Quick
            test_waits_off_the_domain;
          Alcotest.test_case "closed" `Quick test_closed;
          Alcotest.test_case "foreign keys enforced" `Quick test_foreign_keys_on;
          Alcotest.test_case "a cancelled fold" `Quick test_cancelled_fold;
          Alcotest.test_case "no value logged" `Quick test_no_value_logged;
        ] );
      ( "transactions",
        [
          Alcotest.test_case "commit and rollback" `Quick
            test_commit_and_rollback;
          Alcotest.test_case "inside another" `Quick test_nested;
          Alcotest.test_case "rolled back by SQLite" `Quick
            test_rolled_back_by_sqlite;
          Alcotest.test_case "a deferred conflict" `Quick test_deferred_conflict;
          Alcotest.test_case "a commit under a reader" `Quick test_commit_busy;
          Alcotest.test_case "retries" `Quick test_retries;
          Alcotest.test_case "behaviours" `Quick test_behaviours;
          Alcotest.test_case "a raise and a cancellation" `Quick
            test_raise_and_cancel;
        ] );
      ( "a load",
        [
          Alcotest.test_case "copy_in" `Quick test_copy_in;
          Alcotest.test_case "copy_in fails whole" `Quick test_copy_in_failures;
        ] );
      ( "a pool",
        [
          Alcotest.test_case "lends and takes back" `Quick test_pool;
          Alcotest.test_case "busy" `Quick test_pool_busy;
          Alcotest.test_case "lends clean" `Quick test_pool_clean;
          Alcotest.test_case "closed" `Quick test_pool_closed;
          Alcotest.test_case "no pool in memory" `Quick test_pool_memory;
        ] );
      ( "verify",
        [
          Alcotest.test_case "statements checked" `Quick test_verify;
          Alcotest.test_case "each branch" `Quick test_verify_each_branch;
        ] );
      ( "wal and the pool",
        [
          Alcotest.test_case "readers beside a writer" `Quick
            test_wal_readers_beside_a_writer;
          Alcotest.test_case "a deferred write after a read" `Quick
            test_wal_a_deferred_write_after_a_read;
          Alcotest.test_case "a pool on domains" `Quick test_pool_on_domains;
          Alcotest.test_case "a closed connection made again" `Quick
            test_pool_remakes_a_closed_connection;
          Alcotest.test_case "a cancelled borrow" `Quick
            test_pool_cancelled_borrow;
        ] );
      ( "edges",
        [
          Alcotest.test_case "any text read or refused" `Quick
            test_any_text_is_read_or_refused;
          Alcotest.test_case "times at their edges" `Quick
            test_times_at_their_edges;
          Alcotest.test_case "NaN is NULL" `Quick test_nan_is_null;
          Alcotest.test_case "copy_in cancelled" `Quick test_copy_in_cancelled;
          Alcotest.test_case "exclusive keeps readers out" `Quick
            test_exclusive_keeps_readers_out;
          Alcotest.test_case "keep when COMMIT fails" `Quick
            test_keep_when_commit_fails;
        ] );
    ]

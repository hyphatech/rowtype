(* Every scalar against Postgres at its edges, read both ways a cell
   arrives: in binary, on a connection that keeps statements, and in text,
   on one that keeps none. The two are separate decoders, so a value is
   only proved when it comes back through each. *)

module S = Rowtype
module Pg = Rowtype_postgres

let ok = function
  | Ok v -> v
  | Error e -> Alcotest.failf "unexpected error: %s" (S.error_to_string e)

(* The two ways a cell arrives, each a connection of its own on a database
   of the case's own. *)
let formats = [ ("binary", None); ("text", Some 0) ]

let on_each_format setup f =
  Db_target.with_postgres (fun target ->
      List.iter
        (fun (format, statement_cache) ->
          let db = Db_target.connect ?statement_cache target in
          Fun.protect
            ~finally:(fun () -> Pg.close db)
            (fun () ->
              List.iter (fun s -> ok (Pg.exec_raw db s)) setup;
              f format db))
        formats)

(* A value written to a column of [column]'s type and read back, on a
   table of its own so the formats do not see each other's rows, named for
   the type: a connection keeps a statement with its parameter's type, so
   one insert's text over two types would bind the second as the first. *)
let back db ~column ty v =
  let table =
    "v_"
    ^ String.map
        (function ('a' .. 'z' | '0' .. '9') as c -> c | _ -> '_')
        column
  in
  ok
    (Pg.exec_raw db
       (Printf.sprintf "drop table if exists %s; create table %s (x %s)" table
          table column));
  ok
    (Pg.run db
       (S.exec ~params:ty (Printf.sprintf "insert into %s values ($1)" table))
       v);
  ok
    (Pg.run db
       (S.find ~params:S.unit ~row:ty (Printf.sprintf "select x from %s" table))
       ())

let round_trips ~column ty testable values =
  on_each_format [] (fun format db ->
      List.iter
        (fun v ->
          Alcotest.check testable
            (Printf.sprintf "%s, in %s" column format)
            v (back db ~column ty v))
        values)

(* What a statement reading [sql] answers as [ty], or the refusal. *)
let reads db ty sql = Pg.run db (S.find ~params:S.unit ~row:ty sql) ()

let refused what = function
  | Ok _ -> Alcotest.failf "%s was read" what
  | Error (`Db _) -> ()
  | Error (`Conflict _ | `Not_serializable _ | `Closed | `Lost _) ->
      Alcotest.failf "%s: not a decoding failure" what

let test_integers () =
  round_trips ~column:"int8" S.int Alcotest.int
    [ 0; 1; -1; 42; max_int; min_int ];
  round_trips ~column:"int4" S.int Alcotest.int
    [ 2_147_483_647; -2_147_483_648 ];
  round_trips ~column:"int2" S.int Alcotest.int [ 32_767; -32_768 ];
  (* An int8 past OCaml's 63 bits is refused, never wrapped into another
     number. *)
  on_each_format [] (fun format db ->
      refused
        (format ^ ": an int8 past 63 bits")
        (reads db S.int "select 9223372036854775807::int8");
      refused
        (format ^ ": the lowest int8")
        (reads db S.int "select (-9223372036854775808)::int8"))

let exact = Alcotest.testable (fun f x -> Format.fprintf f "%h" x) Float.equal

let test_floats () =
  round_trips ~column:"float8" S.float exact
    [
      0.;
      -0.;
      1.5;
      0.1;
      Float.nan;
      Float.infinity;
      Float.neg_infinity;
      Float.max_float;
      Float.min_float;
      4.9e-324;
      -1e300;
    ];
  (* A float4 holds single precision: what comes back is the single nearest
     the value, the same in either format. *)
  let single x = Int32.float_of_bits (Int32.bits_of_float x) in
  round_trips ~column:"float4" S.float exact [ 1.5; 0.; Float.infinity ];
  on_each_format [] (fun format db ->
      Alcotest.check exact
        (format ^ ": 0.1 in a float4")
        (single 0.1)
        (back db ~column:"float4" S.float 0.1))

let test_text () =
  round_trips ~column:"text" S.text Alcotest.string
    [
      "";
      "plain";
      "a \"quote\", a \\backslash, a 'tick'";
      "new\nline\ttab";
      "Ωμέγα 😀 中文";
      "NULL";
      String.make 1_000_000 'x';
    ];
  round_trips ~column:"varchar(8)" S.text Alcotest.string [ "eight ch" ];
  round_trips ~column:"char(3)" S.text Alcotest.string [ "abc" ];
  (* Postgres text holds no NUL: the server refuses it, and says so. *)
  on_each_format [] (fun format db ->
      ok (Pg.exec_raw db "drop table if exists v; create table v (x text)");
      refused
        (format ^ ": a NUL in text")
        (Pg.run db (S.exec ~params:S.text "insert into v values ($1)") "a\000b"))

let test_bytes () =
  let every_byte = String.init 256 Char.chr in
  round_trips ~column:"bytea" S.bytes Alcotest.string
    [ ""; every_byte; "\000"; String.make 1_000_000 '\255' ]

let test_bools () =
  round_trips ~column:"bool" S.bool Alcotest.bool [ true; false ]

(* Microseconds since the epoch, before it as after, read the same in a
   session whose time zone is not UTC -- to a day inside years 1 and 9999,
   past which such a session prints a year no reader of text can read. *)
let test_instants () =
  on_each_format [ "set time zone 'America/St_Johns'" ] (fun format db ->
      List.iter
        (fun us ->
          Alcotest.(check int)
            (Printf.sprintf "%d, in %s" us format)
            us
            (back db ~column:"timestamptz" Db_target.instant_us us))
        [
          0;
          1;
          -1;
          1_758_620_000_123_456;
          -62_135_510_400_000_000 (* 0001-01-02 *);
          253_402_214_399_999_999 (* 9999-12-30 23:59:59.999999 *);
        ];
      refused (format ^ ": infinity")
        (reads db S.instant "select 'infinity'::timestamptz"))

(* An int8 over its whole range, where [S.int] is a bit short of it. *)
let test_int64s () =
  round_trips ~column:"int8" S.int64 Alcotest.int64
    [ 0L; 1L; -1L; Int64.max_int; Int64.min_int ];
  on_each_format [] (fun format db ->
      Alcotest.(check (result int64 reject))
        (format ^ ": an int4, widened")
        (Ok (-2_147_483_648L))
        (reads db S.int64 "select (-2147483648)::int4"))

(* A date at the edges of the years read, a leap day among them. *)
let test_dates () =
  round_trips ~column:"date" S.date
    Alcotest.(triple int int int)
    [ (2026, 9, 27); (1, 1, 1); (9999, 12, 31); (2024, 2, 29) ];
  on_each_format [] (fun format db ->
      refused (format ^ ": infinity")
        (reads db S.date "select 'infinity'::date"))

(* A timestamp is a reading on no clock: the same reading comes back in a
   session whose time zone is not UTC. *)
let test_timestamps () =
  on_each_format [ "set time zone 'America/St_Johns'" ] (fun format db ->
      List.iter
        (fun us ->
          Alcotest.(check int)
            (Printf.sprintf "%d, in %s" us format)
            us
            (back db ~column:"timestamp" Db_target.timestamp_us us))
        [ 0; 1; -1; 1_758_620_000_123_456 ];
      Alcotest.(check (result int reject))
        (format ^ ": read as the reading it is")
        (Ok 1_758_620_000_123_456)
        (reads db Db_target.timestamp_us
           "select timestamp '2025-09-23 09:33:20.123456'"))

(* An interval keeps its months, days and microseconds apart, each signed on
   its own, through a session whose own style would print it otherwise. *)
let test_intervals () =
  let interval =
    Alcotest.testable
      (fun f (i : S.interval) ->
        Format.fprintf f "%d mons %d days %d us" i.months i.days i.microseconds)
      (fun (a : S.interval) b ->
        a.months = b.months && a.days = b.days
        && a.microseconds = b.microseconds)
  in
  round_trips ~column:"interval" S.interval interval
    [
      { months = 14; days = 3; microseconds = 14_706_789_000 };
      { months = -14; days = 3; microseconds = -14_706_000_000 };
      { months = 1; days = -1; microseconds = 0 };
      { months = 0; days = 0; microseconds = -1_500_000 };
      { months = 0; days = 0; microseconds = 0 };
    ];
  on_each_format [] (fun format db ->
      Alcotest.(check (result interval reject))
        (format ^ ": a month less a day is not 29 days")
        (Ok { months = 1; days = -1; microseconds = 0 })
        (reads db S.interval "select interval '1 mon -1 day'"))

let test_uuids () =
  round_trips ~column:"uuid" Db_target.uuid_text Alcotest.string
    [
      "00000000-0000-0000-0000-000000000000";
      "0190c0fe-7a1b-7c3d-8e4f-a0b1c2d3e4f5";
    ];
  on_each_format [] (fun format db ->
      Alcotest.(check string)
        (format ^ ": written in capitals, read in lower case")
        "ffffffff-ffff-ffff-ffff-ffffffffffff"
        (back db ~column:"uuid" Db_target.uuid_text
           "FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF"))

let test_json () =
  let doc = {|{"a": [1, 2.5, null], "b": "Ω", "c": {"d": true}}|} in
  round_trips ~column:"json" S.json Alcotest.string
    [ doc; "[]"; "null"; "\"x\"" ];
  on_each_format [] (fun format db ->
      Alcotest.(check string)
        (format ^ ": jsonb, as Postgres keeps it")
        {|{"a": [1, 2.5, null], "b": "Ω", "c": {"d": true}}|}
        (back db ~column:"jsonb" S.json doc))

(* Arrays travel as text in both formats, so the encoder and the reader of
   that text are the whole of it: any list of strings, quotes, braces,
   backslashes, commas and the word NULL among them, must come back as
   itself, NULLs included. *)
let hostile =
  let open QCheck.Gen in
  let piece =
    oneof_list
      [
        "\""; "\\"; ","; "{"; "}"; " "; "NULL"; "null"; ""; "Ω"; "😀"; "a"; "\n";
      ]
  in
  let element = map (String.concat "") (list_size (int_range 0 6) piece) in
  list_size (int_range 0 8) (option element)

let test_arrays_of_any_text () =
  Db_target.with_postgres (fun target ->
      let connections =
        List.map
          (fun (_, statement_cache) ->
            Db_target.connect ?statement_cache target)
          formats
      in
      Fun.protect
        ~finally:(fun () -> List.iter Pg.close connections)
        (fun () ->
          let ty = S.array (S.opt S.text) in
          let echo = S.find ~params:ty ~row:ty "select $1::text[]" in
          QCheck.Test.check_exn
            (QCheck.Test.make ~count:500
               ~name:"an array of any text comes back as itself"
               (QCheck.make
                  ~print:(fun xs ->
                    String.concat "; "
                      (List.map
                         (function
                           | None -> "NULL" | Some s -> Printf.sprintf "%S" s)
                         xs))
                  hostile)
               (fun xs ->
                 List.for_all
                   (fun db ->
                     List.equal
                       (Option.equal String.equal)
                       xs
                       (ok (Pg.run db echo xs)))
                   connections))))

let test_arrays_of_bytes () =
  let ty = S.array S.bytes in
  on_each_format [] (fun format db ->
      let xs = [ ""; String.init 256 Char.chr; "\"{,}\\" ] in
      Alcotest.(check (list string))
        (format ^ ": every byte, in an array")
        xs
        (ok (Pg.run db (S.find ~params:ty ~row:ty "select $1::bytea[]") xs)))

(* A second dimension has no list to read into, and is refused. *)
let test_a_second_dimension_is_refused () =
  on_each_format [] (fun format db ->
      refused
        (format ^ ": a two-dimensional array")
        (reads db (S.array S.int) "select array[[1, 2], [3, 4]]");
      refused
        (format ^ ": a two-dimensional array with bounds")
        (reads db (S.array S.int) "select '[0:1][1:2]={{1,2},{3,4}}'::int[]"))

(* A list has no lower bound: an array that starts anywhere but 1 is read
   in order, as one that starts at 1 is. *)
let test_any_lower_bound_is_read () =
  on_each_format [] (fun format db ->
      Alcotest.(check (list int))
        (format ^ ": from 0") [ 1; 2 ]
        (ok (reads db (S.array S.int) "select '[0:1]={1,2}'::int[]"));
      Alcotest.(check (list (option string)))
        (format ^ ": from -3, holding what a bound is written with")
        [ Some "[1:2]="; None; Some "}" ]
        (ok
           (reads db
              (S.array (S.opt S.text))
              "select '[-3:-1]={\"[1:2]=\",NULL,\"}\"}'::text[]")))

(* An element that does not read is named by its column and its place,
   counted from 1 as Postgres counts them. *)
let test_an_element_is_named () =
  on_each_format [] (fun format db ->
      let said ty sql =
        match reads db ty sql with
        | Error (`Db m) -> m
        | Ok _ | Error (`Conflict _ | `Not_serializable _ | `Closed | `Lost _)
          ->
            Alcotest.failf "%s: %s was read" format sql
      in
      Alcotest.(check string)
        (format ^ ": a NULL")
        "column 2, element 2: NULL where a value was expected"
        (said S.(t2 int (array int)) "select 0, array[1, null]");
      Alcotest.(check string)
        (format ^ ": not its type")
        "column 1, element 3: expected INT, got an element of an array of type \
         1016"
        (said (S.array S.int) "select array[1, 2, 9223372036854775807]::int8[]"))

(* An element is read as its type alone is: a [real] rounded to single
   precision as a [real] column is, and a [text] refused as an [int] as a
   [text] column is, whatever its text says. *)
let test_an_element_is_its_type () =
  on_each_format [] (fun format db ->
      Alcotest.(check (list (float 0.)))
        (format ^ ": 0.1 in a real[], as in a real")
        [ ok (reads db S.float "select 0.1::real") ]
        (ok (reads db (S.array S.float) "select array[0.1::real]"));
      refused
        (format ^ ": a text[] as an array of int")
        (reads db (S.array S.int) "select array['42'::text]");
      refused
        (format ^ ": a text[] as an array of bool")
        (reads db (S.array S.bool) "select array['t'::text]");
      refused
        (format ^ ": an int4[] as an array of JSON")
        (reads db (S.array S.json) "select array[1::int4]"))

(* A type the database made has no oid that says what it is, so it is read
   through a cast, and a read without one says so. *)
let test_an_enum_is_read_through_a_cast () =
  on_each_format
    [ "drop type if exists mood"; "create type mood as enum ('calm', 'cross')" ]
    (fun format db ->
      Alcotest.(check string)
        (format ^ ": cast") "calm"
        (ok (reads db S.text "select 'calm'::mood::text"));
      Alcotest.(check (list string))
        (format ^ ": an array, cast")
        [ "calm"; "cross" ]
        (ok (reads db (S.array S.text) "select '{calm,cross}'::mood[]::text[]"));
      List.iter
        (fun (what, cast, said) ->
          match said with
          | Error (`Db m) ->
              Alcotest.(check bool)
                (format ^ ": " ^ what ^ " says to cast: " ^ m)
                true
                (String.ends_with ~suffix:("cast any other, as " ^ cast) m)
          | Ok _ | Error (`Conflict _ | `Not_serializable _ | `Closed | `Lost _)
            ->
              Alcotest.failf "%s: %s was read" format what)
        [
          ( "an enum",
            "::text",
            Result.map ignore (reads db S.text "select 'calm'::mood") );
          ( "an array of one",
            "::text[]",
            Result.map ignore
              (reads db (S.array S.text) "select '{calm}'::mood[]") );
        ];
      refused
        (format ^ ": an enum as JSON")
        (reads db S.json "select 'calm'::mood");
      refused
        (format ^ ": a numeric as an int")
        (reads db S.int "select 1::numeric"))

type mood = Calm | Cross

let mood_label = function Calm -> "calm" | Cross -> "cross"
let mood = S.enum mood_label [ Calm; Cross ]

let moods =
  Alcotest.testable
    (fun f m -> Format.pp_print_string f (mood_label m))
    (fun a b -> String.equal (mood_label a) (mood_label b))

(* Labels an array's text quotes, and one it would read as NULL unquoted. *)
let odd_labels = [ "a b"; "x,y"; "q\"q"; "back\\slash"; "{"; "NULL"; "it's" ]

(* An enum is read by its labels with no cast, alone and in an array, as
   the label it is in binary and in text; and a label no value has, a
   composite's text, and text that is no enum are refused. *)
let test_an_enum_is_read_by_its_labels () =
  on_each_format
    [
      "drop type if exists mood, odd, pair cascade";
      "create type mood as enum ('calm', 'cross')";
      "create type odd as enum ("
      ^ String.concat ", "
          (List.map
             (fun l ->
               "'" ^ String.concat "''" (String.split_on_char '\'' l) ^ "'")
             odd_labels)
      ^ ")";
      "create type pair as (a int, b int)";
    ]
    (fun format db ->
      List.iter
        (fun m ->
          Alcotest.check moods (format ^ ": a mood") m
            (back db ~column:"mood" mood m))
        [ Calm; Cross ];
      Alcotest.(check (list moods))
        (format ^ ": an array of moods")
        [ Cross; Calm; Cross ]
        (back db ~column:"mood[]" (S.array mood) [ Cross; Calm; Cross ]);
      let odd = S.enum Fun.id odd_labels in
      List.iter
        (fun l ->
          Alcotest.(check string)
            (format ^ ": " ^ l)
            l
            (back db ~column:"odd" odd l))
        odd_labels;
      Alcotest.(check (list string))
        (format ^ ": an array of odd labels")
        odd_labels
        (back db ~column:"odd[]" (S.array odd) odd_labels);
      refused
        (format ^ ": a label no value has")
        (reads db (S.enum mood_label [ Calm ]) "select 'cross'::mood");
      refused (format ^ ": a composite")
        (reads db mood "select row(1, 2)::pair");
      refused (format ^ ": text") (reads db mood "select 'calm'::text"))

let () =
  Db_target.required ~suite:"types";
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Db_target.eio env ~sw;
  Alcotest.run ~and_exit:false "types"
    [
      ( "scalars",
        [
          Alcotest.test_case "integers" `Quick test_integers;
          Alcotest.test_case "floats" `Quick test_floats;
          Alcotest.test_case "text" `Quick test_text;
          Alcotest.test_case "bytes" `Quick test_bytes;
          Alcotest.test_case "bools" `Quick test_bools;
          Alcotest.test_case "instants" `Quick test_instants;
          Alcotest.test_case "int64s" `Quick test_int64s;
          Alcotest.test_case "dates" `Quick test_dates;
          Alcotest.test_case "timestamps" `Quick test_timestamps;
          Alcotest.test_case "intervals" `Quick test_intervals;
          Alcotest.test_case "uuids" `Quick test_uuids;
          Alcotest.test_case "JSON" `Quick test_json;
        ] );
      ( "arrays",
        [
          Alcotest.test_case "any text comes back as itself" `Quick
            test_arrays_of_any_text;
          Alcotest.test_case "bytes" `Quick test_arrays_of_bytes;
          Alcotest.test_case "a second dimension is refused" `Quick
            test_a_second_dimension_is_refused;
          Alcotest.test_case "any lower bound is read" `Quick
            test_any_lower_bound_is_read;
          Alcotest.test_case "an element is named" `Quick
            test_an_element_is_named;
          Alcotest.test_case "an element is its type" `Quick
            test_an_element_is_its_type;
          Alcotest.test_case "an enum is read through a cast" `Quick
            test_an_enum_is_read_through_a_cast;
          Alcotest.test_case "an enum is read by its labels" `Quick
            test_an_enum_is_read_by_its_labels;
        ] );
    ]

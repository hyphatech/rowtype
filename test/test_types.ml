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
   table of its own so the formats do not see each other's rows. *)
let back db ~column ty v =
  ok
    (Pg.exec_raw db
       (Printf.sprintf "drop table if exists v; create table v (x %s)" column));
  ok (Pg.run db (S.exec ~params:ty "insert into v values ($1)") v);
  ok (Pg.run db (S.find ~params:S.unit ~row:ty "select x from v") ())

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
  | Error (`Conflict _ | `Not_serializable _) ->
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
        (reads db (S.array S.int) "select array[[1, 2], [3, 4]]"))

(* An element that does not read is named by its column and its place,
   counted from 1 as Postgres counts them. *)
let test_an_element_is_named () =
  on_each_format [] (fun format db ->
      let said ty sql =
        match reads db ty sql with
        | Error (`Db m) -> m
        | Ok _ | Error (`Conflict _ | `Not_serializable _) ->
            Alcotest.failf "%s: %s was read" format sql
      in
      Alcotest.(check string)
        (format ^ ": a NULL")
        "column 1, element 2: NULL where a value was expected"
        (said S.(t2 int (array int)) "select 0, array[1, null]");
      Alcotest.(check string)
        (format ^ ": not its type")
        "column 0, element 3: expected INT, got text that is not one"
        (said (S.array S.int) "select array['1', '2', 'x']"))

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
          Alcotest.test_case "an element is named" `Quick
            test_an_element_is_named;
        ] );
    ]

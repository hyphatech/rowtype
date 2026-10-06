(* Every scalar against every column type a database has, read as a column
   and bound as a parameter: verify accepts a pair exactly where the scalar
   reads the type, and refuses every other, a type no scalar reads among
   them. *)

module S = Rowtype
module Pg = Rowtype_postgres

let ok = function
  | Ok v -> v
  | Error e -> Alcotest.failf "unexpected error: %s" (S.error_to_string e)

(* Each column type, as the table and a cast name it. *)
let types =
  [
    "int2";
    "int4";
    "int8";
    "oid";
    "float4";
    "float8";
    "text";
    "varchar";
    "char(3)";
    "name";
    "bytea";
    "bool";
    "timestamptz";
    "uuid";
    "json";
    "jsonb";
    "mood";
    "positive";
    "numeric";
    "date";
    "timestamp";
    "interval";
    "int4[]";
  ]

type scalar = Scalar : string * 'a S.ty * string list -> scalar

(* Each scalar, and the types it reads: everything else it must refuse. *)
let scalars =
  [
    Scalar ("int", S.int, [ "int2"; "int4"; "int8"; "oid"; "positive" ]);
    Scalar ("float", S.float, [ "float4"; "float8" ]);
    Scalar
      ( "text",
        S.text,
        [
          "text"; "varchar"; "char(3)"; "name"; "uuid"; "json"; "jsonb"; "mood";
        ] );
    Scalar ("bytes", S.bytes, [ "bytea" ]);
    Scalar ("bool", S.bool, [ "bool" ]);
    Scalar ("int64", S.int64, [ "int2"; "int4"; "int8"; "oid"; "positive" ]);
    Scalar ("instant", S.instant, [ "timestamptz" ]);
    Scalar ("date", S.date, [ "date" ]);
    Scalar ("timestamp", S.timestamp, [ "timestamp" ]);
    Scalar ("interval", S.interval, [ "interval" ]);
    Scalar ("uuid", S.uuid, [ "uuid" ]);
    Scalar ("json", S.json, [ "json"; "jsonb" ]);
    Scalar ("an array of int", S.array S.int, [ "int4[]" ]);
  ]

let column_name t =
  String.map (function '(' | ')' | '[' | ']' -> '_' | c -> c) t

let setup =
  [
    "create type mood as enum ('calm', 'cross')";
    "create domain positive as int check (value > 0)";
    "create table m ("
    ^ String.concat ", "
        (List.map (fun t -> Printf.sprintf "c_%s %s" (column_name t) t) types)
    ^ ")";
  ]

let test_every_scalar_against_every_type () =
  Db_target.with_postgres (fun target ->
      let db = Db_target.connect target in
      Fun.protect
        ~finally:(fun () -> Pg.close db)
        (fun () ->
          List.iter (fun s -> ok (Pg.exec_raw db s)) setup;
          List.iter
            (fun (Scalar (name, ty, reads)) ->
              List.iter
                (fun t ->
                  let expected = List.mem t reads in
                  let column =
                    S.find ~params:S.unit ~row:ty
                      (Printf.sprintf "select c_%s from m" (column_name t))
                  and parameter =
                    S.exec ~params:ty
                      (Printf.sprintf "update m set c_%s = $1" (column_name t))
                  in
                  List.iter
                    (fun (side, statement) ->
                      Alcotest.(check bool)
                        (Printf.sprintf "%s %s a %s %s" name
                           (if expected then "reads" else "does not read")
                           t side)
                        expected
                        (match Pg.verify db [ statement ] with
                        | Ok () -> true
                        | Error (`Disagreements _) -> false
                        | Error (#S.error as e) ->
                            Alcotest.failf "the check could not run: %s"
                              (S.error_to_string e)))
                    [ ("column", S.Any column); ("parameter", S.Any parameter) ])
                types)
            scalars))

let () =
  Db_target.required ~suite:"verify";
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Db_target.eio env ~sw;
  Alcotest.run ~and_exit:false "verify"
    [
      ( "types",
        [
          Alcotest.test_case "every scalar against every type" `Quick
            test_every_scalar_against_every_type;
        ] );
    ]

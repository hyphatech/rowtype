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

(* Each scalar, the types it reads, and the ones it only writes: an enum
   takes a label bound as text, and is read through a cast. *)
type scalar = Scalar : string * 'a S.ty * string list * string list -> scalar

(* Everything else a scalar must refuse. *)
let scalars =
  [
    Scalar ("int", S.int, [ "int2"; "int4"; "int8"; "oid"; "positive" ], []);
    Scalar ("float", S.float, [ "float4"; "float8" ], []);
    Scalar
      ( "text",
        S.text,
        [ "text"; "varchar"; "char(3)"; "name"; "uuid"; "json"; "jsonb" ],
        [ "mood" ] );
    Scalar ("bytes", S.bytes, [ "bytea" ], []);
    Scalar ("bool", S.bool, [ "bool" ], []);
    Scalar ("int64", S.int64, [ "int2"; "int4"; "int8"; "oid"; "positive" ], []);
    Scalar ("instant", S.instant, [ "timestamptz" ], []);
    Scalar ("date", S.date, [ "date" ], []);
    Scalar ("timestamp", S.timestamp, [ "timestamp" ], []);
    Scalar ("interval", S.interval, [ "interval" ], []);
    Scalar ("uuid", S.uuid, [ "uuid" ], []);
    Scalar ("json", S.json, [ "json"; "jsonb" ], []);
    Scalar ("an array of int", S.array S.int, [ "int4[]" ], []);
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
            (fun (Scalar (name, ty, reads, written)) ->
              List.iter
                (fun t ->
                  let column =
                    S.find ~params:S.unit ~row:ty
                      (Printf.sprintf "select c_%s from m" (column_name t))
                  and parameter =
                    S.exec ~params:ty
                      (Printf.sprintf "update m set c_%s = $1" (column_name t))
                  in
                  List.iter
                    (fun (side, statement, expected) ->
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
                    [
                      ("column", S.Any column, List.mem t reads);
                      ( "parameter",
                        S.Any parameter,
                        List.mem t reads || List.mem t written );
                    ])
                types)
            scalars))

(* One value of each type, which the scalars that read text read too, so a
   read refused is refused for its type and not its text. *)
let values =
  [
    "1";
    "1";
    "1";
    "1";
    "1.5";
    "1.5";
    "'1'";
    "'1'";
    "'1'";
    "'1'";
    "'\\x01'";
    "true";
    "now()";
    "gen_random_uuid()";
    "'1'";
    "'1'";
    "'calm'";
    "1";
    "1";
    "current_date";
    "now()";
    "'1 day'";
    "'{1}'";
  ]

(* What [verify] says is what a read does: a pair it accepts reads, and a
   pair it refuses is refused, in binary and in text alike. *)
let test_a_read_agrees_with_the_check () =
  Db_target.with_postgres (fun target ->
      let setup_db = Db_target.connect target in
      Fun.protect
        ~finally:(fun () -> Pg.close setup_db)
        (fun () ->
          List.iter (fun s -> ok (Pg.exec_raw setup_db s)) setup;
          ok
            (Pg.exec_raw setup_db
               ("insert into m values (" ^ String.concat ", " values ^ ")")));
      let reads_in format statement_cache =
        let db = Db_target.connect ?statement_cache target in
        Fun.protect
          ~finally:(fun () -> Pg.close db)
          (fun () ->
            List.concat_map
              (fun (Scalar (name, ty, reads, _)) ->
                List.filter_map
                  (fun t ->
                    let expected = List.mem t reads in
                    let read =
                      match
                        Pg.run db
                          (S.find ~params:S.unit ~row:ty
                             (Printf.sprintf "select c_%s from m"
                                (column_name t)))
                          ()
                      with
                      | Ok _ -> true
                      | Error (`Db _) -> false
                      | Error
                          (( `Conflict _ | `Not_serializable _ | `Closed
                           | `Lost _ ) as e) ->
                          Alcotest.failf "the read could not run: %s"
                            (S.error_to_string e)
                    in
                    if Bool.equal read expected then None
                    else
                      Some
                        (Printf.sprintf "%s: %s %s a %s" format name
                           (if read then "reads" else "does not read")
                           t))
                  types)
              scalars)
      in
      Alcotest.(check (list string))
        "every read the check does not expect" []
        (reads_in "binary" None @ reads_in "text" (Some 0)))

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
          Alcotest.test_case "a read agrees with the check" `Quick
            test_a_read_agrees_with_the_check;
        ] );
    ]

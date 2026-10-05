(* The walk of a shape, with no database: a backend that answers every
   statement with one row, its own parameters, so whatever a shape binds it
   must decode again. A shape and a value made at random must come back as
   themselves, which is the property everything above the walk relies on. *)

module S = Rowtype

(* A cell as this backend keeps one: a scalar's text, or an array's
   elements. *)
type cell = V of string | L of cell option list

module Echo = struct
  type conn = unit
  type param = cell option
  type nonrec cell = cell
  type failure = string

  let null = None

  let param : type a. a S.scalar -> a -> param =
   fun s v ->
    Some
      (V
         (match s with
         | S.Int -> string_of_int v
         | S.Instant -> Ptime.to_rfc3339 ~frac_s:12 v
         | S.Float -> Printf.sprintf "%h" v
         | S.Text -> v
         | S.Bytes -> v
         | S.Uuid -> v
         | S.Json -> v
         | S.Bool -> string_of_bool v))

  let read : type a. a S.scalar -> cell -> (a, string) result =
   fun s c ->
    let some what = function Some x -> Ok x | None -> Error what in
    match c with
    | L _ -> Error "an array where a scalar was expected"
    | V v -> (
        match s with
        | S.Int -> some "an int" (int_of_string_opt v)
        | S.Instant -> (
            match Ptime.of_rfc3339 v with
            | Ok (t, _, _) -> Ok t
            | Error _ -> Error "an instant")
        | S.Float -> some "a float" (float_of_string_opt v)
        | S.Text -> Ok v
        | S.Bytes -> Ok v
        | S.Uuid -> Ok v
        | S.Json -> Ok v
        | S.Bool -> some "a bool" (bool_of_string_opt v))

  let fold () _ params ~init ~row = Ok (row init (Array.of_list params), 1)
  let script () _ = Ok ()
  let array elements = Some (L elements)

  let elements = function
    | L elements -> Ok elements
    | V _ -> Error "a scalar where an array was expected"

  let commit () = Ok `Committed
  let error m = `Db m
end

module Run = S.Make (Echo)

(* A shape, how to make its values, compare two and print one, and whether
   a value of it is NULL in every column it spans -- worked out from the
   shape's structure, never by asking the walk under test. *)
type 'a shape = {
  ty : 'a S.ty;
  gen : 'a QCheck.Gen.t;
  equal : 'a -> 'a -> bool;
  print : 'a -> string;
  name : string;
  all_null : 'a -> bool;
}

type any_shape = Shape : 'a shape -> any_shape

let scalar ty gen equal print name =
  Shape { ty; gen; equal; print; name; all_null = (fun _ -> false) }

(* Any instant Ptime holds, to the picosecond: a day of its range and a
   moment of the day. *)
let instant =
  let open QCheck.Gen in
  let first, _ = Ptime.Span.to_d_ps (Ptime.to_span Ptime.min) in
  let last, _ = Ptime.Span.to_d_ps (Ptime.to_span Ptime.max) in
  map2
    (fun d ps ->
      Option.value ~default:Ptime.epoch
        (Option.bind (Ptime.Span.of_d_ps (d, ps)) Ptime.of_span))
    (int_range first last)
    (map Int64.of_int (int_range 0 (86_400_000_000_000 - 1)))

let scalars =
  let open QCheck.Gen in
  [
    scalar S.int int Int.equal string_of_int "int";
    scalar S.float float Float.equal (Printf.sprintf "%h") "float";
    scalar S.text string String.equal (Printf.sprintf "%S") "text";
    scalar S.bytes string String.equal (Printf.sprintf "%S") "bytes";
    scalar S.bool bool Bool.equal string_of_bool "bool";
    scalar S.instant instant Ptime.equal (Ptime.to_rfc3339 ~frac_s:12) "instant";
    scalar S.uuid string String.equal (Printf.sprintf "%S") "uuid";
    scalar S.json string String.equal (Printf.sprintf "%S") "json";
  ]

let pair_of (Shape a) (Shape b) =
  Shape
    {
      ty = S.t2 a.ty b.ty;
      gen = QCheck.Gen.pair a.gen b.gen;
      equal = (fun (x, y) (x', y') -> a.equal x x' && b.equal y y');
      print = (fun (x, y) -> Printf.sprintf "(%s, %s)" (a.print x) (b.print y));
      name = Printf.sprintf "t2 (%s) (%s)" a.name b.name;
      all_null = (fun (x, y) -> a.all_null x && b.all_null y);
    }

(* A value NULL in every column of its option is, to a row, [None]: the one
   case a value does not come back as itself, which [S.opt] says. *)
let opt_of (Shape a) =
  let as_a_row = function Some x when a.all_null x -> None | v -> v in
  Shape
    {
      ty = S.opt a.ty;
      gen = QCheck.Gen.option a.gen;
      equal = (fun x y -> Option.equal a.equal (as_a_row x) (as_a_row y));
      print = (function None -> "None" | Some x -> "Some " ^ a.print x);
      name = Printf.sprintf "opt (%s)" a.name;
      all_null = (function None -> true | Some x -> a.all_null x);
    }

let array_of (Shape a) =
  Shape
    {
      ty = S.array (S.opt a.ty);
      gen = QCheck.Gen.list_small (QCheck.Gen.option a.gen);
      equal = List.equal (Option.equal a.equal);
      print =
        (fun xs ->
          "["
          ^ String.concat "; "
              (List.map (function None -> "NULL" | Some x -> a.print x) xs)
          ^ "]");
      name = Printf.sprintf "array (opt (%s))" a.name;
      all_null = (fun _ -> false);
    }

let conv_of (Shape a) =
  Shape
    {
      ty = S.conv a.ty ~of_:(fun x -> `Wrapped x) ~to_:(fun (`Wrapped x) -> x);
      gen = QCheck.Gen.map (fun x -> `Wrapped x) a.gen;
      equal = (fun (`Wrapped x) (`Wrapped y) -> a.equal x y);
      print = (fun (`Wrapped x) -> "Wrapped " ^ a.print x);
      name = Printf.sprintf "conv (%s)" a.name;
      all_null = (fun (`Wrapped x) -> a.all_null x);
    }

let rec any_shape depth =
  let open QCheck.Gen in
  let leaf = oneof_list scalars in
  if depth = 0 then leaf
  else
    let inner = any_shape (depth - 1) in
    oneof_weighted
      [
        (3, leaf);
        (2, map2 pair_of inner inner);
        (1, map opt_of inner);
        (1, map array_of leaf);
        (1, map conv_of inner);
      ]

(* A shape and a value of it, made together. *)
type sample = Sample : 'a shape * 'a -> sample

let sample =
  QCheck.make
    ~print:(fun (Sample (s, v)) -> Printf.sprintf "%s: %s" s.name (s.print v))
    QCheck.Gen.(
      let* (Shape s) = any_shape 3 in
      let* v = s.gen in
      return (Sample (s, v)))

let round_trip =
  QCheck.Test.make ~count:5000 ~name:"a value comes back as itself" sample
    (fun (Sample (s, v)) ->
      match Run.run () (S.find ~params:s.ty ~row:s.ty "echo") v with
      | Ok v' -> s.equal v v'
      | Error e -> QCheck.Test.fail_reportf "%s" (S.error_to_string e))

(* An option over a shape whose own first column may be NULL: the columns
   after it are still the value, and reading the first alone dropped them. *)
let test_an_option_reads_every_column () =
  let ty = S.(opt (t2 (opt int) int)) in
  let back v = Run.run () (S.find ~params:ty ~row:ty "echo") v in
  Alcotest.(check (result (option (pair (option int) int)) reject))
    "a NULL first, a value after it"
    (Ok (Some (None, 5)))
    (back (Some (None, 5)));
  Alcotest.(check (result (option (pair (option int) int)) reject))
    "NULL in every column" (Ok None) (back None)

(* An option over no column has no NULL to read, so every value of it is
   NULL in all of its columns, and reads back as [None], as [S.opt] says. *)
let test_an_option_over_no_column_is_none () =
  let ty = S.opt S.unit in
  let back v = Run.run () (S.find ~params:ty ~row:ty "echo") v in
  Alcotest.(check (result (option unit) reject)) "None" (Ok None) (back None);
  Alcotest.(check (result (option unit) reject))
    "Some (), NULL in every column it has" (Ok None) (back (Some ()))

(* An array's element is one column and no array of its own: a list of
   lists has no one-dimensional form, and is refused before anything is
   sent or read, an empty one included. *)
let test_an_array_of_arrays_is_refused () =
  let ty = S.array (S.array S.int) in
  let refused what = function
    | Error (`Db m) ->
        Alcotest.(check string)
          what "an array's element is one column and no array of its own" m
    | Error (`Conflict _ | `Not_serializable _) ->
        Alcotest.failf "%s: not the shape's error" what
    | Ok _ -> Alcotest.failf "%s: passed" what
  in
  refused "bound" (Run.bind ty [ [ 1; 2 ] ]);
  refused "bound, empty" (Run.bind ty []);
  refused "read"
    (Run.run () (S.find ~params:(S.array S.int) ~row:ty "echo") [ 1; 2 ])

let () =
  Alcotest.run "shape"
    [
      ( "the walk",
        [
          QCheck_alcotest.to_alcotest round_trip;
          Alcotest.test_case "an option reads every column" `Quick
            test_an_option_reads_every_column;
          Alcotest.test_case "an option over no column is None" `Quick
            test_an_option_over_no_column_is_none;
          Alcotest.test_case "an array of arrays is refused" `Quick
            test_an_array_of_arrays_is_refused;
        ] );
    ]

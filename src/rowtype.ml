let ( let* ) = Result.bind

type error =
  [ `Conflict of string option | `Not_serializable of string | `Db of string ]

let error_to_string : [< error ] -> string = function
  | `Conflict (Some name) -> Printf.sprintf "the constraint %s refused it" name
  | `Conflict None -> "a constraint refused it"
  | `Not_serializable m -> m
  | `Db m -> m

(* ------------------------------------------------------------------ *)
(* Shapes, re-exported so a call site writes [S.int]; the .mli makes the
   type abstract, so a caller builds a shape and never takes one apart. *)

type interval = Shape.interval = {
  months : int;
  days : int;
  microseconds : int;
}

type 'a scalar = 'a Shape.scalar =
  | Int : int scalar
  | Int64 : int64 scalar
  | Float : float scalar
  | Text : string scalar
  | Bytes : string scalar
  | Bool : bool scalar
  | Instant : Ptime.t scalar
  | Date : Ptime.date scalar
  | Timestamp : Ptime.t scalar
  | Interval : interval scalar
  | Uuid : Uuidm.t scalar
  | Json : string scalar

type 'a ty = 'a Shape.ty

let unit = Shape.unit
let int = Shape.int
let int64 = Shape.int64
let float = Shape.float
let text = Shape.text
let bytes = Shape.bytes
let bool = Shape.bool
let instant = Shape.instant
let date = Shape.date
let timestamp = Shape.timestamp
let interval = Shape.interval
let uuid = Shape.uuid
let json = Shape.json
let opt = Shape.opt
let array = Shape.array
let t2 = Shape.t2
let t3 = Shape.t3
let t4 = Shape.t4
let t5 = Shape.t5
let t6 = Shape.t6
let t7 = Shape.t7
let t8 = Shape.t8
let t9 = Shape.t9
let t10 = Shape.t10
let t11 = Shape.t11
let conv = Shape.conv
let parse = Shape.parse
let arity = Shape.arity

(* ------------------------------------------------------------------ *)
(* Statements *)

(* How many rows a statement answers, and so what running it gives: one
   runner reads it, so the walk over a result is written once. *)
type _ rows =
  | Nothing : unit rows
  | One : 'r ty -> 'r rows
  | At_most_one : 'r ty -> 'r option rows
  | Every : 'r ty -> 'r list rows
  | Count : int rows

type ('p, 'r) statement = { sql : string; params : 'p ty; rows : 'r rows }

let exec ~params sql = { sql; params; rows = Nothing }
let find ~params ~row sql = { sql; params; rows = One row }
let find_opt ~params ~row sql = { sql; params; rows = At_most_one row }
let list ~params ~row sql = { sql; params; rows = Every row }
let exec_count ~params sql = { sql; params; rows = Count }

(* ------------------------------------------------------------------ *)
(* Backends *)

module type Backend = sig
  type conn
  type param
  type cell
  type failure

  val null : param
  val param : 'a scalar -> 'a -> param
  val read : 'a scalar -> cell -> ('a, string) result

  val fold :
    conn ->
    string ->
    param list ->
    init:'acc ->
    row:('acc -> cell option array -> 'acc) ->
    ('acc * int, failure) result

  val script : conn -> string -> (unit, failure) result
  val array : param list -> param
  val elements : cell -> (cell option list, string) result
  val commit : conn -> ([ `Committed | `Rolled_back ], failure) result
  val error : failure -> error
end

module type S = sig
  type conn

  val run : conn -> ('p, 'r) statement -> 'p -> ('r, [> error ]) result

  val fold :
    conn ->
    ('p, 'r list) statement ->
    'p ->
    init:'acc ->
    ('acc -> 'r -> 'acc) ->
    ('acc, [> error ]) result

  val exec_raw : conn -> string -> (unit, [> error ]) result
  val commit : conn -> ([ `Committed | `Rolled_back ], [> error ]) result
end

(* The walk exists to pattern-match the GADT, and qualifying every
   constructor would bury the one thing it does. *)
open Shape

let rec holds_array : type a. a ty -> bool = function
  | Unit | Scalar _ -> false
  | Array _ -> true
  | Opt t -> holds_array t
  | Pair (a, b) -> holds_array a || holds_array b
  | Conv (t, _, _) -> holds_array t
  | Parse (t, _, _) -> holds_array t

(* An array's element is one column and no array of its own: a list of
   lists has no one-dimensional form to send or read. *)
let element t = arity t = 1 && not (holds_array t)

module Make (B : Backend) = struct
  type conn = B.conn

  (* The walk answers the closed type; what leaves is opened, so a caller's
     own failures can join it. *)
  let widen = function Ok v -> Ok v | Error (#error as e) -> Error e

  let not_element =
    `Db "an array's element is one column and no array of its own"

  let rec encode : type a. a ty -> a -> (B.param list, error) result =
   fun ty v ->
    match (ty, v) with
    | Unit, () -> Ok []
    | Scalar s, x -> Ok [ B.param s x ]
    | Opt t, Some x -> encode t x
    | Opt t, None -> Ok (List.init (arity t) (fun _ -> B.null))
    | Pair (a, b), (x, y) ->
        let* xs = encode a x in
        let* ys = encode b y in
        Ok (xs @ ys)
    | Conv (t, _, to_), x -> encode t (to_ x)
    | Parse (t, _, to_), x -> encode t (to_ x)
    | Array t, xs ->
        if not (element t) then Error not_element
        else
          let* elements =
            List.fold_right
              (fun x acc ->
                let* acc = acc in
                let* e = encode t x in
                match e with [ p ] -> Ok (p :: acc) | _ -> Error not_element)
              xs (Ok [])
          in
          Ok [ B.array elements ]

  (* A shape wider than the result asks for a column the row does not
     have: the caller's error, said as one. *)
  let column ~name (cells : B.cell option array) i =
    if i >= Array.length cells then
      Error
        (`Db
           (Printf.sprintf "%s: the result has only %d columns" (name i)
              (Array.length cells)))
    else Ok cells.(i)

  let refused ~name i m = Error (`Db (Printf.sprintf "%s: %s" (name i) m))

  let non_null ~name cells i =
    let* cell = column ~name cells i in
    match cell with
    | None -> refused ~name i "NULL where a value was expected"
    | Some c -> Ok c

  (* Counted from 1, as a person counts them and as a backend's check of
     statements names them. *)
  let row_column i = Printf.sprintf "column %d" (i + 1)

  (* The decoded value and the next column index, so [Pair] can walk a row
     without the shapes needing to know their own offsets. [name] says where
     a failure is: a column of the row, or an element of one. A NULL is
     refused by every scalar alike, text included: a column that may hold
     one is [opt]'s. *)
  let rec decode : type a.
      name:(int -> string) ->
      a ty ->
      B.cell option array ->
      int ->
      (a * int, error) result =
   fun ~name ty cells i ->
    match ty with
    | Unit -> Ok ((), i)
    | Scalar s -> (
        let* c = non_null ~name cells i in
        match B.read s c with
        | Ok v -> Ok (v, i + 1)
        | Error m -> refused ~name i m)
    (* [None] only where every column the option spans is NULL: a shape
       inside it may hold a NULL of its own in its first column, and
       reading that as [None] would drop the columns after it. *)
    | Opt t ->
        let n = arity t in
        let rec all_null k =
          k = n
          ||
          match column ~name cells (i + k) with
          | Ok None -> all_null (k + 1)
          | Ok (Some _) | Error _ -> false
        in
        if all_null 0 then Ok (None, i + n)
        else
          let* v, j = decode ~name t cells i in
          Ok (Some v, j)
    | Pair (a, b) ->
        let* x, j = decode ~name a cells i in
        let* y, k = decode ~name b cells j in
        Ok ((x, y), k)
    | Conv (t, of_, _) ->
        let* x, j = decode ~name t cells i in
        Ok (of_ x, j)
    | Parse (t, of_, _) -> (
        let* x, j = decode ~name t cells i in
        match of_ x with
        | Some y -> Ok (y, j)
        | None -> refused ~name i "unreadable value")
    | Array t -> (
        let* c = non_null ~name cells i in
        if not (element t) then Error not_element
        else
          match B.elements c with
          | Error m -> refused ~name i m
          | Ok elements ->
              (* Counted from 1, as a row's columns are. *)
              let rec each k acc = function
                | [] -> Ok (List.rev acc, i + 1)
                | e :: rest ->
                    let element _ =
                      Printf.sprintf "%s, element %d" (name i) k
                    in
                    let* v, _ = decode ~name:element t [| e |] 0 in
                    each (k + 1) (v :: acc) rest
              in
              each 1 [] elements)

  (* A row that does not decode is the answer, and the rows after it are
     passed over rather than stopped: the backend is reading them already,
     and leaving its reply half-read would leave its connection mid-reply. *)
  let fold_cells db st args ~init ~row =
    let* params = encode st.params args in
    match
      B.fold db st.sql params ~init:(Ok init) ~row:(fun acc cells ->
          match acc with Error _ -> acc | Ok acc -> row acc cells)
    with
    | Ok (answer, _) -> answer
    | Error f -> Error (B.error f)

  let decode_row ty cells = Result.map fst (decode ~name:row_column ty cells 0)

  (* The first row, decoded, and how many there were: [find] and [find_opt]
     count, because a statement that promised one row and answered several
     is wrong, and saying how many is what finds it. *)
  let first_of db st args ty =
    fold_cells db st args ~init:(None, 0) ~row:(fun (first, n) cells ->
        match first with
        | Some _ -> Ok (first, n + 1)
        | None ->
            let* v = decode_row ty cells in
            Ok (Some v, n + 1))

  let answered count expected =
    Error
      (`Db
         (Printf.sprintf "the statement answered %d rows, not %s" count expected))

  let answer : type p r. conn -> (p, r) statement -> p -> (r, error) result =
   fun db st args ->
    match st.rows with
    | Nothing -> fold_cells db st args ~init:() ~row:(fun () _ -> Ok ())
    | At_most_one ty -> (
        let* first, count = first_of db st args ty in
        match first with
        | Some _ when count > 1 -> answered count "at most one"
        | Some _ | None -> Ok first)
    | One ty -> (
        let* first, count = first_of db st args ty in
        match first with
        | Some v when count = 1 -> Ok v
        | Some _ | None -> answered count "one")
    | Every ty ->
        Result.map List.rev
          (fold_cells db st args ~init:[] ~row:(fun acc cells ->
               let* v = decode_row ty cells in
               Ok (v :: acc)))
    | Count -> (
        let* params = encode st.params args in
        match B.fold db st.sql params ~init:() ~row:(fun () _ -> ()) with
        | Ok ((), count) -> Ok count
        | Error f -> Error (B.error f))

  (* Only two statements answer a list: [list]'s, whose rows are folded as
     they arrive, and a [find] whose one row is an array, which is already
     whole. *)
  let fold_list : type p r acc.
      conn ->
      (p, r list) statement ->
      p ->
      init:acc ->
      (acc -> r -> acc) ->
      (acc, error) result =
   fun db st args ~init f ->
    match st.rows with
    | Every ty ->
        fold_cells db st args ~init ~row:(fun acc cells ->
            let* v = decode_row ty cells in
            Ok (f acc v))
    | One _ -> Result.map (List.fold_left f init) (answer db st args)

  let bind ty v = widen (encode ty v)
  let run db st args = widen (answer db st args)
  let fold db st args ~init f = widen (fold_list db st args ~init f)
  let exec_raw db sql = widen (Result.map_error B.error (B.script db sql))
  let commit db = widen (Result.map_error B.error (B.commit db))
end

(* ------------------------------------------------------------------ *)
(* What a statement declares *)

type any = Any : ('p, 'r) statement -> any
type column = Column : 'a scalar -> column | Array_of of column list

let rec columns : type a. a ty -> column list = function
  | Unit -> []
  | Scalar s -> [ Column s ]
  | Opt t -> columns t
  | Pair (a, b) -> columns a @ columns b
  | Conv (t, _, _) -> columns t
  | Parse (t, _, _) -> columns t
  | Array t -> [ Array_of (columns t) ]

type declared = {
  sql : string;
  parameters : column list;
  row : column list option;
}

let declared (Any st) =
  {
    sql = st.sql;
    parameters = columns st.params;
    row =
      (match st.rows with
      | Nothing | Count -> None
      | One t -> Some (columns t)
      | At_most_one t -> Some (columns t)
      | Every t -> Some (columns t));
  }

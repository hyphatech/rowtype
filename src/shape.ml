(* One value drives both directions, binding and decoding, because the two
   written apart are where they come to disagree. *)

(* What one column holds, and all a backend is asked to encode and read. *)
type _ scalar =
  | Int : int scalar
  | Float : float scalar
  | Text : string scalar
  | Bytes : string scalar
  | Bool : bool scalar
  (* An instant, as Ptime's on this side, so a unit is never a caller's to
     get wrong, and whatever the database keeps one as on the other. *)
  | Instant : Ptime.t scalar
  (* A uuid as Uuidm's, and a JSON document in its own text: types of their
     own so a backend reads each exactly and checks a column is one. *)
  | Uuid : Uuidm.t scalar
  | Json : string scalar

type _ ty =
  | Unit : unit ty
  | Scalar : 'a scalar -> 'a ty
  | Opt : 'a ty -> 'a option ty
  | Pair : 'a ty * 'b ty -> ('a * 'b) ty
  (* An isomorphism, so flat tuples and records can be expressed in terms
     of nested pairs without the call site ever seeing the nesting. *)
  | Conv : 'a ty * ('a -> 'b) * ('b -> 'a) -> 'b ty
  (* The same, for a value the database may hold and this program may not
     be able to read -- an enum label, a key -- so a row that does not
     decode is an error that says which column, never a raise. *)
  | Parse : 'a ty * ('a -> 'b option) * ('b -> 'a) -> 'b ty
  (* One column holding many values, each of the element's shape -- which
     is one column itself, or the statement is refused. *)
  | Array : 'a ty -> 'a list ty

let unit = Unit
let int = Scalar Int
let float = Scalar Float
let text = Scalar Text
let bytes = Scalar Bytes
let bool = Scalar Bool
let instant = Scalar Instant
let uuid = Scalar Uuid
let json = Scalar Json
let opt t = Opt t
let array t = Array t
let t2 a b = Pair (a, b)

let t3 a b c =
  Conv
    ( Pair (a, Pair (b, c)),
      (fun (x, (y, z)) -> (x, y, z)),
      fun (x, y, z) -> (x, (y, z)) )

let t4 a b c d =
  Conv
    ( Pair (a, Pair (b, Pair (c, d))),
      (fun (w, (x, (y, z))) -> (w, x, y, z)),
      fun (w, x, y, z) -> (w, (x, (y, z))) )

let t5 a b c d e =
  Conv
    ( Pair (a, Pair (b, Pair (c, Pair (d, e)))),
      (fun (v, (w, (x, (y, z)))) -> (v, w, x, y, z)),
      fun (v, w, x, y, z) -> (v, (w, (x, (y, z)))) )

let t6 a b c d e f =
  Conv
    ( Pair (a, Pair (b, Pair (c, Pair (d, Pair (e, f))))),
      (fun (u, (v, (w, (x, (y, z))))) -> (u, v, w, x, y, z)),
      fun (u, v, w, x, y, z) -> (u, (v, (w, (x, (y, z))))) )

let t7 a b c d e f g =
  Conv
    ( Pair (a, Pair (b, Pair (c, Pair (d, Pair (e, Pair (f, g)))))),
      (fun (t, (u, (v, (w, (x, (y, z)))))) -> (t, u, v, w, x, y, z)),
      fun (t, u, v, w, x, y, z) -> (t, (u, (v, (w, (x, (y, z)))))) )

let t8 a b c d e f g h =
  Conv
    ( Pair (a, Pair (b, Pair (c, Pair (d, Pair (e, Pair (f, Pair (g, h))))))),
      (fun (s, (t, (u, (v, (w, (x, (y, z))))))) -> (s, t, u, v, w, x, y, z)),
      fun (s, t, u, v, w, x, y, z) -> (s, (t, (u, (v, (w, (x, (y, z))))))) )

let t9 a b c d e f g h i =
  Conv
    ( Pair
        ( a,
          Pair (b, Pair (c, Pair (d, Pair (e, Pair (f, Pair (g, Pair (h, i)))))))
        ),
      (fun (r, (s, (t, (u, (v, (w, (x, (y, z)))))))) ->
        (r, s, t, u, v, w, x, y, z)),
      fun (r, s, t, u, v, w, x, y, z) ->
        (r, (s, (t, (u, (v, (w, (x, (y, z)))))))) )

let t10 a b c d e f g h i j =
  Conv
    ( Pair
        ( a,
          Pair
            ( b,
              Pair
                ( c,
                  Pair (d, Pair (e, Pair (f, Pair (g, Pair (h, Pair (i, j))))))
                ) ) ),
      (fun (q, (r, (s, (t, (u, (v, (w, (x, (y, z))))))))) ->
        (q, r, s, t, u, v, w, x, y, z)),
      fun (q, r, s, t, u, v, w, x, y, z) ->
        (q, (r, (s, (t, (u, (v, (w, (x, (y, z))))))))) )

let t11 a b c d e f g h i j k =
  Conv
    ( Pair
        ( a,
          Pair
            ( b,
              Pair
                ( c,
                  Pair
                    ( d,
                      Pair
                        (e, Pair (f, Pair (g, Pair (h, Pair (i, Pair (j, k))))))
                    ) ) ) ),
      (fun (p, (q, (r, (s, (t, (u, (v, (w, (x, (y, z)))))))))) ->
        (p, q, r, s, t, u, v, w, x, y, z)),
      fun (p, q, r, s, t, u, v, w, x, y, z) ->
        (p, (q, (r, (s, (t, (u, (v, (w, (x, (y, z)))))))))) )

let conv shape ~of_ ~to_ = Conv (shape, of_, to_)
let parse shape ~of_ ~to_ = Parse (shape, of_, to_)

let rec arity : type a. a ty -> int = function
  | Unit -> 0
  | Scalar _ -> 1
  | Opt t -> arity t
  | Pair (a, b) -> arity a + arity b
  | Conv (t, _, _) -> arity t
  | Parse (t, _, _) -> arity t
  | Array _ -> 1

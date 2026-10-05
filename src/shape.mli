(** The shape of a query.

    A {!ty} describes a query's shape {i once} and drives both directions --
    binding parameters and decoding rows. This module holds the description and
    nothing that runs one; {!Rowtype} walks it against a backend.

    {b Library-internal.} The constructors are exposed because the walk has to
    pattern-match a shape. {!Rowtype} re-exports the combinators and
    re-abstracts the type, and that is the face a caller gets. *)

(** {1 Shapes} *)

type interval = { months : int; days : int; microseconds : int }

type _ scalar =
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

type _ ty =
  | Unit : unit ty
  | Scalar : 'a scalar -> 'a ty
  | Opt : 'a ty -> 'a option ty
  | Pair : 'a ty * 'b ty -> ('a * 'b) ty
  | Conv : 'a ty * ('a -> 'b) * ('b -> 'a) -> 'b ty
  | Parse : 'a ty * ('a -> 'b option) * ('b -> 'a) -> 'b ty
  | Array : 'a ty -> 'a list ty

val unit : unit ty
val int : int ty
val int64 : int64 ty
val float : float ty
val text : string ty
val bytes : string ty
val bool : bool ty
val instant : Ptime.t ty
val date : Ptime.date ty
val timestamp : Ptime.t ty
val interval : interval ty
val uuid : Uuidm.t ty
val json : string ty
val opt : 'a ty -> 'a option ty
val array : 'a ty -> 'a list ty
val t2 : 'a ty -> 'b ty -> ('a * 'b) ty
val t3 : 'a ty -> 'b ty -> 'c ty -> ('a * 'b * 'c) ty
val t4 : 'a ty -> 'b ty -> 'c ty -> 'd ty -> ('a * 'b * 'c * 'd) ty

val t5 :
  'a ty -> 'b ty -> 'c ty -> 'd ty -> 'e ty -> ('a * 'b * 'c * 'd * 'e) ty

val t6 :
  'a ty ->
  'b ty ->
  'c ty ->
  'd ty ->
  'e ty ->
  'f ty ->
  ('a * 'b * 'c * 'd * 'e * 'f) ty

val t7 :
  'a ty ->
  'b ty ->
  'c ty ->
  'd ty ->
  'e ty ->
  'f ty ->
  'g ty ->
  ('a * 'b * 'c * 'd * 'e * 'f * 'g) ty

val t8 :
  'a ty ->
  'b ty ->
  'c ty ->
  'd ty ->
  'e ty ->
  'f ty ->
  'g ty ->
  'h ty ->
  ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) ty

val t9 :
  'a ty ->
  'b ty ->
  'c ty ->
  'd ty ->
  'e ty ->
  'f ty ->
  'g ty ->
  'h ty ->
  'i ty ->
  ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i) ty

val t10 :
  'a ty ->
  'b ty ->
  'c ty ->
  'd ty ->
  'e ty ->
  'f ty ->
  'g ty ->
  'h ty ->
  'i ty ->
  'j ty ->
  ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j) ty

val t11 :
  'a ty ->
  'b ty ->
  'c ty ->
  'd ty ->
  'e ty ->
  'f ty ->
  'g ty ->
  'h ty ->
  'i ty ->
  'j ty ->
  'k ty ->
  ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j * 'k) ty

val conv : 'a ty -> of_:('a -> 'b) -> to_:('b -> 'a) -> 'b ty
(** Map a shape onto a record or variant, so a row decodes straight into the
    type the rest of the code wants. *)

val parse : 'a ty -> of_:('a -> 'b option) -> to_:('b -> 'a) -> 'b ty
(** As {!conv}, for a value a row may hold that this program cannot read. *)

val arity : 'a ty -> int
(** How many SQL columns or placeholders a shape occupies. An option is
    transparent -- [opt (t2 int int)] is still two columns. *)

(** A query's shape, described once, for any SQL database.

    One {!ty} value describes a query's shape {i once} and drives both
    directions -- binding parameters and decoding rows -- and a {!statement} is
    a query with its shapes and how many rows it answers, declared once and run
    by name:

    {[
    let orders =
      S.list ~params:S.int
        ~row:(S.t2 S.int (S.opt S.text))
        "select id, note from orders where customer_id = $1 order by id"

    let notes db customer = Pg.run db orders customer
    (* : ((int * string option) list, _) result *)
    ]}

    Running one is a backend's: {!Make} over a {!Backend} gives the functions
    that run statements on that database's connection, and the database, its
    connections and its driver are the backend's business, never this library's.
    It links no database's library and reads no SQL: a statement reaches the
    database as written, in its own syntax, placeholders included -- [$1], [$2],
    ... for Postgres, bound by position from the shape's values, and [$1] twice
    to ask about one value twice. Whether a statement and its values agree in
    number is the database's to check.

    Deliberately small: no connection pooling and no prepared-statement cache.
    Pooling belongs to whatever owns the connections, where the scheduler is;
    the rest is weight this does not use. *)

type error =
  [ `Conflict of string option
    (** a constraint refused the write because of other rows -- a unique key, an
        exclusion, a foreign key -- named where the database says which:
        distinct from [`Db] because it is the caller's answer to give -- an HTTP
        layer wants 409 here, not 500 -- and named because a table with two
        unique columns has two different answers. A constraint on the row alone,
        NOT NULL or CHECK, refuses the program's own mistake, and is [`Db]. *)
  | `Not_serializable of string
    (** the database rolled the transaction back because it could not be ordered
        with a concurrent one -- a serialization failure, or a deadlock -- in
        its words: running the transaction again may succeed, which is the one
        thing no other failure here says *)
  | `Closed
    (** the connection was closed before the statement was sent -- by its owner,
        or by a failure before it -- so nothing was sent and nothing it would
        have done was done *)
  | `Lost of string
    (** the connection failed with the statement sent and no answer back -- a
        timeout, a socket that broke, a server that ended the session -- in
        words for a log: what the statement did is unknown, and a write may have
        been applied. Inside a transaction, a statement lost before its [COMMIT]
        did nothing, since an open transaction ends with its connection; only a
        lost [COMMIT] may have committed. *)
  | `Db of string  (** anything else, in words for a log *) ]
(** A polymorphic variant, so a statement's failures join the ones a caller adds
    around it -- a pool's, a transaction's, the work's own -- in one result:
    every function that fails answers [[> error]]. *)

val error_to_string : [< error ] -> string
(** In words for a log, which never hold a parameter's value: a conflict by its
    constraint's name, anything else as it was told. *)

(** {1 Shapes}

    A shape is built from the scalar combinators and the tuple ones. Flat
    [t3]..[t11] are isomorphisms over nested pairs, so a call site never sees
    the nesting. *)

type 'a ty

val unit : unit ty
val int : int ty

val int64 : int64 ty
(** An integer over the whole of a 64-bit column's range, which OCaml's [int] is
    a bit short of. *)

val float : float ty

val text : string ty
(** A NULL is refused, as by every scalar; a column that may hold one is
    [opt text]. *)

val bytes : string ty
(** Any bytes -- a digest, a ciphertext -- kept as the database keeps binary, a
    [bytea] in Postgres, so none of it is encoded in the SQL. *)

val bool : bool ty

val instant : Ptime.t ty
(** An instant, as a [Ptime.t], so no caller converts a unit by hand: kept as
    the database keeps an instant, and its backend says to what precision and
    over which years. *)

val date : Ptime.date ty
(** A date, as Ptime's [(year, month, day)]. *)

val timestamp : Ptime.t ty
(** A timestamp with no zone, a reading on no clock: the instant whose reading
    in UTC it is, so a caller who knows its zone moves it by that offset. *)

type interval = { months : int; days : int; microseconds : int }
(** An interval as SQL keeps one: months, days and microseconds apart, each
    signed on its own, since a month is no fixed number of days and a day,
    across a change of clocks, no fixed number of hours. *)

val interval : interval ty

val uuid : Uuidm.t ty
(** A [uuid], as a [Uuidm.t]. *)

val json : string ty
(** A [json] or [jsonb] document, as its text; parsing it is the caller's. *)

val opt : 'a ty -> 'a option ty
(** [None] binds NULL in every column the shape spans, and decodes from a row
    where every one of them is NULL. A value that is itself NULL in every column
    -- [Some None] of [opt (opt int)], or [Some ()] of [opt unit], which spans
    none -- reads back as [None], since a row cannot tell the two apart. *)

val array : 'a ty -> 'a list ty
(** One column holding a list, each value of the element's shape: [array int]
    for a [smallint[]], [array (opt float)] for a [real[]] that holds NULLs,
    [array key] for keys read through their own parse. An element is one column
    and no array of its own -- one dimension, as a list has -- and any other is
    the statement's [`Db] error, which a backend's check of statements names
    before any runs. A list has no first index: an array is read in order from
    wherever the database starts it, and one written starts where the database
    starts a new one. *)

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
(** As {!conv}, for a value the database may hold and this program may not be
    able to read -- an enum label, a key. A row whose value [of_] refuses is a
    decode error naming the column, exactly as a type mismatch is. *)

val arity : 'a ty -> int
(** How many SQL columns or placeholders a shape occupies. An option is
    transparent -- [opt (t2 int int)] is still two columns. *)

(** {1 Statements}

    A statement is its SQL, the shape of its parameters and how many rows it
    answers, in one value: declared once -- beside the one function that runs
    it, or inline where it runs once -- and run by a backend's [run], whose
    answer is the type the statement says. *)

type ('p, 'r) statement
(** A statement taking ['p] and answering ['r]. *)

val exec : params:'p ty -> string -> ('p, unit) statement
(** A statement whose rows, if it has any, are discarded. *)

val find : params:'p ty -> row:'r ty -> string -> ('p, 'r) statement
(** Exactly one row: for a statement the database guarantees answers one -- an
    [insert ... returning], an aggregate. None or several is an [`Db] error
    saying how many, so a lookup that may find nothing is {!find_opt}'s, or a
    missing row answers as a failure. *)

val find_opt : params:'p ty -> row:'r ty -> string -> ('p, 'r option) statement
(** The one row, if any. Several is a [`Db] error saying how many, as for
    {!find}: a lookup that may match more than one row is {!list}'s, or a
    [limit 1] says which. *)

val list : params:'p ty -> row:'r ty -> string -> ('p, 'r list) statement
(** Every row, in the order the database sent them. *)

val exec_count : params:'p ty -> string -> ('p, int) statement
(** How many rows the statement changed. Any rows it answers are discarded.

    What makes a guarded write an {i answer} rather than a write:
    ["update ... where owner is null"] run as this is "did I get it", decided by
    the database in one statement, where a read and then a write is two. *)

(** {1 What a statement declares}

    For a check against the database: every statement of a program, in one list,
    and what each says it takes and answers. *)

type any =
  | Any : ('p, 'r) statement -> any
      (** A statement, whatever it takes and answers. *)

(** {1 Backends} *)

(** What one column holds: all a backend is asked to encode and read. *)
type _ scalar =
  | Int : int scalar
  | Int64 : int64 scalar
  | Float : float scalar
  | Text : string scalar
  | Bytes : string scalar  (** a string of any bytes, a [bytea] in Postgres *)
  | Bool : bool scalar
  | Instant : Ptime.t scalar
      (** an instant, kept however the database keeps one -- a [timestamptz] in
          Postgres -- so an instant means the same to every backend *)
  | Date : Ptime.date scalar
  | Timestamp : Ptime.t scalar  (** a reading on no clock, as {!timestamp} *)
  | Interval : interval scalar
  | Uuid : Uuidm.t scalar  (** a uuid *)
  | Json : string scalar  (** a JSON document, as its text *)

(** A database, as {!Make} needs it. *)
module type Backend = sig
  type conn
  (** A connection, which the backend opens, closes and serialises. *)

  type param
  (** One value bound to a statement. *)

  type cell
  (** One non-NULL column of a row, as the database sent it. *)

  type failure
  (** What the database or the connection answered instead of a result; told as
      {!type-error} by [error]. *)

  val null : param
  (** A NULL, bound to one placeholder. *)

  val param : 'a scalar -> 'a -> param
  (** A value bound to one placeholder, as the scalar says to send it. *)

  val read : 'a scalar -> cell -> ('a, string) result
  (** A cell as a scalar, or what was expected and what came instead, in words.
  *)

  val fold :
    conn ->
    string ->
    param list ->
    init:'acc ->
    row:('acc -> cell option array -> 'acc) ->
    ('acc * int, failure) result
  (** Run one statement with its parameters and fold its rows as they arrive,
      [None] for a NULL, answering the fold and how many rows the statement
      changed, or sent where it changes none, as the database counts them. *)

  val batch : conn -> string -> param list list -> (unit, failure) result
  (** Run one statement once for each list of parameters, in one round trip and
      as one: if any run fails, none applies. Rows are discarded. *)

  val script : conn -> string -> (unit, failure) result
  (** Run statements with no parameters, several if the text has several, and
      discard their rows. *)

  val array : param list -> param
  (** Elements bound as one array, in the database's own form of one. *)

  val elements : cell -> (cell option list, string) result
  (** An array's elements, in order, [None] for a NULL; [Error] for what is not
      one, or has more than one dimension, in words. *)

  val commit : conn -> ([ `Committed | `Rolled_back ], failure) result
  (** [COMMIT], and what the database made of it. *)

  val error : failure -> error
  (** The failure told as {!type-error}: a constraint refusing the write because
      of other rows is [`Conflict], naming the constraint where the database
      does; a connection closed before the statement was sent is [`Closed], and
      one that failed with it in flight [`Lost]; and anything else is [`Db] in
      words for a log -- never a parameter's value. *)
end

(** What {!Make} gives: statements run on a backend's connection. *)
module type S = sig
  type conn

  val run : conn -> ('p, 'r) statement -> 'p -> ('r, [> error ]) result
  (** [run db statement args]: the statement with its parameters, answering what
      it says it answers. *)

  val fold :
    conn ->
    ('p, 'r list) statement ->
    'p ->
    init:'acc ->
    ('acc -> 'r -> 'acc) ->
    ('acc, [> error ]) result
  (** [fold db statement args ~init f]: what {!run} answers, folded as
      [List.fold_left] would fold it, without the list being made -- each row of
      a {!Rowtype.list} statement is decoded and handed to [f] as it arrives, so
      a result of any size is read in the memory of one row. A row that does not
      decode is the answer, and [f] sees none after it. *)

  val run_many :
    conn -> ('p, unit) statement -> 'p list -> (unit, [> error ]) result
  (** [run_many db statement values]: an {!exec} statement run once for each
      value, in one round trip and all or nothing -- if any run fails, none
      applies, and the answer is that failure. No values sends nothing. A
      statement that answers a row, a {!find} of [unit], is [`Db] before
      anything is sent, since a batch reads no rows. *)

  val exec_raw : conn -> string -> (unit, [> error ]) result
  (** For DDL and for control statements ([begin], [commit]): no parameters, and
      rows are discarded. Several statements may be sent in one call. *)

  val commit : conn -> ([ `Committed | `Rolled_back ], [> error ]) result
  (** [COMMIT], and what the database made of it: [`Rolled_back] for a
      transaction a failed statement had already aborted, which a database may
      roll back without an error, and nothing it wrote is kept. *)
end

(** A functor, because one program may use two databases at once, each with its
    own backend. *)
module Make (B : Backend) : sig
  include S with type conn = B.conn

  val bind : 'a ty -> 'a -> (B.param list, [> error ]) result
  (** A value as the parameters {!S.run} binds it to, for a backend's own path
      that takes rows of parameters, such as a bulk load. *)
end

(** One column or placeholder of a shape. *)
type column = Column : 'a scalar -> column | Array_of of column list

type declared = {
  sql : string;  (** as it is sent *)
  parameters : column list;  (** in placeholder order, an option transparent *)
  row : column list option;
      (** each column read, in order; [None] where the rows are discarded --
          {!exec} and {!exec_count} *)
}
(** What one statement says it takes and answers, for a backend's check of it
    against the database. *)

val declared : any -> declared
(** What the statement declares. *)

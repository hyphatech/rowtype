(** {{!Rowtype}rowtype} over SQLite, through the [sqlite3] binding: the one
    place the two meet, and the one package of rowtype that binds C, since
    SQLite is a library, not a server, and has no protocol to speak in OCaml.

    Statements are SQLite's as written. A placeholder is [$1], [$2], ... as in
    every backend, or SQLite's own [?1] or [?], each bound by its number from
    the shape's values, so [$1] twice asks about one value twice. One with a
    name, [:name], is refused, since a shape's values have none, and so are
    placeholders that do not number the values from one, each at least once,
    before anything runs. One statement is run at a time: a text holding several
    is {!exec_raw}'s.

    SQLite keeps each value as an integer, a float, text or a blob, whatever its
    column was declared as, and each scalar is kept as one of them:

    - an [int], an [int64] and a [bool] as an integer, a [bool] [0] or [1];
    - a [float] as a float, and read from an integer too where a float holds it
      exactly; a NaN is kept as NULL, as SQLite keeps one;
    - a [text], a [json] and an enum's label as text, and [bytes] as a blob;
    - an {!Rowtype.instant} as text in UTC, [2026-10-20 09:24:46.621000], the
      form SQLite's date and time functions read and write, to the microsecond,
      so a column of them sorts as time does and compares with
      [datetime('now')]; read with the zone the text gives -- [Z], [+02:00] --
      and in UTC where it gives none, as SQLite reads one, to the microsecond;
    - a {!Rowtype.timestamp} as the same text, a reading on no clock, and one
      that gives a zone is refused;
    - a {!Rowtype.date} as [2026-10-20];
    - a {!Rowtype.val-interval} as ISO 8601 text, [P14M3DT-0.000005S]: months,
      days and seconds, each signed on its own;
    - a {!Rowtype.uuid} as text, [7c9e6679-7425-40de-944b-e07fc1f90ae7], and
      read from a blob of sixteen bytes too;
    - an {!Rowtype.array} as a JSON array, as text, which [json_each] reads in
      the SQL -- [insert into t select value from json_each($1)] -- and
      [json_group_array] writes; a [bytes] element is lowercase hex in it, which
      [unhex] reads.

    A value of another kind than its scalar's is refused, naming the kind and
    never the value. A unique, primary-key or foreign-key violation is
    [`Conflict], named by the columns SQLite names, [users.email], and a foreign
    key's by none, since SQLite names none. [SQLITE_BUSY] -- a lock another
    connection still held when the wait ran out -- is [`Not_serializable]:
    nothing of the statement was done, and running it again may succeed.

    Each connection enforces foreign keys, which SQLite leaves off unless asked.
    Each statement is a [debug] line on [rowtype-sqlite] with how long it took:
    its first line of text, never its parameters. SQLite's calls block, so a
    statement runs on a system thread, as [Eio_unix.run_in_systhread] runs one,
    and its rows are read in chunks of 64, so the domain's other fibers run
    while it waits on the disk or a lock. A connection is one fiber's at a time.
    A fiber cancelled during a statement waits for the chunk being read, then
    the statement is ended and the cancellation passes; a raise from {!fold}'s
    function passes too, and the connection is as usable after either as before.
*)

type conn
(** An open connection. *)

type observer = { around : 'a. string -> (unit -> 'a) -> 'a }
(** What watches every statement a connection runs: [around sql run] is handed
    the statement's text and runs it, answering what [run] answered -- a span
    begun and ended around it, a count. The text has placeholders where the
    values are, and the values are never handed over, since a code, a digest or
    a token is one. Polymorphic, so one observer watches statements of every row
    type. *)

include Rowtype.S with type conn := conn

val copy_in :
  conn ->
  ?schema:string ->
  table:string ->
  columns:string list ->
  'a Rowtype.ty ->
  'a Seq.t ->
  (int, [> Rowtype.error ]) result
(** [copy_in db ~table ~columns row rows] writes the rows with one prepared
    [insert], each bound as {!run} binds a statement's parameters, all of them
    or none, and answers how many were written. The sequence is read as the rows
    are written, a chunk at a time, so a load of any size is never held whole.

    [schema] is the attached database the table is in, [main] unless given.
    [columns] are in the row's order, every column in table order where empty,
    and quoted as given; a number of them that is not the row's arity is refused
    before anything is written. A row that cannot be bound, a constraint and a
    raise from the sequence each fail the whole load, with nothing written; the
    raise passes. Inside a transaction, the load is the transaction's, and goes
    with it. *)

val connect :
  sw:Eio.Switch.t ->
  mono_clock:_ Eio.Time.Mono.t ->
  ?timeout:Mtime.Span.t ->
  ?observe:observer ->
  string ->
  (conn, [> Rowtype.error ]) result
(** [connect ~sw ~mono_clock file] opens the database in [file], making it where
    there is none, as SQLite names one: [":memory:"] is one in memory, this
    connection's alone. [timeout] is how long a statement waits for a lock
    another connection holds before it is [`Not_serializable], 5 s unless given,
    to the millisecond above it. [observe] is around each statement it runs, a
    transaction's [begin] and [commit] among them, and nothing unless given. A
    file that cannot be opened is [`Unsent]. The connection is closed when [sw]
    finishes, unless {!close} closed it first. *)

val close : conn -> unit
(** Close it for good; its next statement is [`Connection `Closed]. A no-op on
    one already closed. *)

(** Connections made up front to one file and lent one at a time. *)
module Pool : sig
  type t

  val create :
    sw:Eio.Switch.t ->
    mono_clock:_ Eio.Time.Mono.t ->
    ?timeout:Mtime.Span.t ->
    ?size:int ->
    ?wait:Mtime.Span.t ->
    ?observe:observer ->
    string ->
    (t, [> Rowtype.error ]) result
  (** [size] connections, 8 unless given and at least one, each as {!connect}
      makes it, [timeout] and [observe] passed through; the first that cannot be
      made is the answer, and those made before it are closed. [wait] is how
      long a borrow waits for one, 5 s unless given. A database in memory,
      [":memory:"] or [""], is refused, since each connection would have one of
      its own: a pool is of a file. Readers alongside a writer want the file in
      WAL mode, [pragma journal_mode = wal], which the file keeps. *)

  type error =
    [ `Pool of
      [ `Busy  (** no connection came free within the wait *)
      | `Closed  (** the pool is closed *) ] ]
  (** What a borrow meets in the pool, a state of the pool's own. *)

  val use : ?wait:Mtime.Span.t -> t -> (conn -> 'a) -> ('a, [> error ]) result
  (** Borrow a connection, run [f] on it on the calling fiber, and give it back,
      even if [f] raises: [`Pool `Busy] if none came free within the wait, and
      [`Pool `Closed] at once if the pool is closed, or closes while the borrow
      waits. A connection is given back with no transaction open: one a borrower
      left open is rolled back. *)

  type stats = {
    size : int;  (** connections the pool keeps *)
    idle : int;  (** of them, not borrowed *)
    waiting : int;  (** borrows waiting for one *)
  }

  val stats : t -> stats
  (** For a health endpoint. *)

  val close : t -> unit
  (** Close the idle connections now and each borrowed one as it is given back.
      A borrow waiting, and every one after, is [`Pool `Closed]. *)
end

(** One transaction on one connection, and its own failures told apart from the
    work's.

    The work answers a [result]: [Ok] commits and [Error] rolls back. A failure
    of the transaction itself joins the work's error type as an open polymorphic
    variant, so a caller whose errors are polymorphic variants, as every
    statement's are, writes nothing, and one whose errors are ordinary variants
    maps once, in its own bracket.

    SQLite's transactions are serializable, so there is no level to choose: what
    is chosen is when a transaction takes the write lock. A statement that fails
    inside one undoes itself alone, as SQLite does, and the transaction goes on;
    SQLite rolls back the whole of it only where it cannot go on -- a full disk,
    an I/O error -- and then its [COMMIT] finds nothing to commit. *)
module Transaction : sig
  (** When the transaction takes the database's write lock, as SQLite's [begin]
      names it: [Deferred] at its first write, [Immediate] as it begins, and
      [Exclusive] as it begins, keeping other connections from reading too
      outside WAL mode. A transaction that reads and then writes begins
      [Immediate], or its first write may find that another connection wrote
      since it read, which is [`Not_serializable] however long it waits. *)
  type behaviour = Deferred | Immediate | Exclusive

  type failure =
    [ `Not_committed of string
      (** the transaction could not begin, or its [COMMIT] failed or found that
          SQLite had rolled it back: in words for the log. Whether it committed
          is known: it did not. *)
    | `Conflict of string option
      (** its [COMMIT] found a deferred foreign key refusing the write, as a
          statement's {!Rowtype.error} names one checked at once, and it was
          rolled back *)
    | `Not_serializable of string
      (** it could not take a lock another connection held -- to begin, or to
          commit -- and its retries did not get past it *) ]
  (** The transaction's own failures, which {!within} adds to the work's: a
      caller tells them from its own as [#Transaction.failure], and names them
      in an error type of its own as [[ my_error | Transaction.failure ]]. *)

  val within :
    ?keep:('e -> bool) ->
    ?behaviour:behaviour ->
    ?retries:int ->
    conn ->
    (conn -> ('a, ([> failure ] as 'e)) result) ->
    ('a, 'e) result
  (** [within db work]: begin, run [work], and commit what it answered [Ok]. A
      refusal rolls back unless [keep] says to keep what it wrote -- a wrong
      sign-in code that has to count its attempt -- and still answers the
      refusal. [keep] keeps nothing unless given.

      [behaviour] is how it begins, [Deferred] unless given, as SQLite begins
      one.

      [retries] is how many times the whole transaction runs again when the work
      answers [`Not_serializable], or its [begin] or [COMMIT] does -- none
      unless given, so a retry is always asked for. Each is an [info] line, and
      the last answer stands. Work that runs again does everything again, so
      work given retries does nothing but its statements.

      [`Not_committed] replaces the work's answer when the transaction could not
      begin -- a transaction already open on the connection, a [within] inside
      another, among the reasons, and the outer one goes on -- when its [COMMIT]
      failed, or when SQLite had rolled it back. [`Conflict] replaces it when a
      deferred foreign key refused the write at [COMMIT], and
      [`Not_serializable] when [COMMIT] could not take its lock; either way the
      transaction is rolled back. Each [`Not_committed] is an [error] line on
      [rowtype-sqlite]. A raise inside [work], a cancellation among them, rolls
      back and passes. *)
end

(** {1 Statements against the database} *)

val verify :
  conn ->
  Rowtype.any list ->
  (unit, [> `Disagreements of string list | Rowtype.error ]) result
(** Every statement prepared without being run, and what SQLite says of each
    compared with what the statement declares: whether it prepares at all, how
    many parameters and, for a statement that reads rows, how many columns, and
    each column read straight from a table by its declared type -- an [integer]
    column as one an [int], an [int64] or a [bool] reads, a [real] one as a
    [float]'s, a [text] one as text's, an instant's or any other kept as text,
    and a [blob] one as [bytes] or a [uuid]. A column made by an expression, or
    declared with no type, holds any kind of value, and is checked where it is
    read. A placeholder with a name is a disagreement, as {!run} refuses one.
    [`Disagreements] is every problem, in words for a person, each naming its
    statement's first line and where in it. *)

type error =
  [ Rowtype.error
  | `Disagreements of string list
  | Pool.error
  | Transaction.failure ]
(** Every failure this backend answers: a statement's, {!verify}'s, a pool's and
    a transaction's, which join in one result -- a transaction in a borrowed
    connection is
    [Result.join (Pool.use pool (fun db -> Transaction.within db work))]. *)

val error_to_string : [< error ] -> string
(** In words for a person, which never hold a parameter's value: every
    disagreement {!verify} found, a pool's state and a transaction's own
    failure, and a statement's failure as {!Rowtype.error_to_string} tells it.
*)

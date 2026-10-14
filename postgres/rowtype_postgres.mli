(** {{!Rowtype}rowtype} over Postgres, through [postgres-eio]: the one place the
    two meet.

    Statements are Postgres's as written, [$1] for a shape's first value. An
    {!Rowtype.instant} is a [timestamptz], which keeps the microsecond too, sent
    as ISO 8601 in UTC and read back whatever the session's time zone, which the
    driver reads in the styles it sets on every connection ([DateStyle=ISO],
    [IntervalStyle=postgres]). Years 1 to 9999: within a day of either end, a
    session far from UTC prints a year that a connection reading text cannot
    read. A value its scalar cannot hold -- infinity, a year outside them, an
    [int8] past an [int]'s 63 bits -- is refused as what it is, saying what
    reads it instead. A [COMMIT] of a transaction a failed statement aborted is
    rolled back without an error, which {!Transaction.within} tells from a
    commit. A result is read in binary wherever the driver reads its type, and
    in text on a connection that keeps no statements, which cannot be bound for
    binary. A unique, exclusion or foreign-key violation is [`Conflict], with
    the constraint the server named, whether a statement or a deferred check at
    [COMMIT] found it.

    Each statement is a [debug] line on [rowtype-postgres] with how long it
    took: its first line of text, never its parameters. A statement runs on the
    calling fiber, as an Eio effect, and a connection is one fiber's at a time.
    A raise from {!fold}'s function passes, and closes the connection, whose
    answer was read only in part: its next statement is [`Connection `Closed]
    until {!revive}. A fiber cancelled with a statement in flight closes its
    connection and asks the server to cancel the statement, waiting a few
    seconds at most for the server to take the request, so nothing runs on for
    nobody. *)

module Conninfo = Postgres_eio.Conninfo
(** What a connection is made to: read from a connection string, either form, by
    [Conninfo.of_string], and moved to another database by
    [Conninfo.with_database]. Its [connect_timeout] bounds each address tried,
    which [Conninfo.with_connect_timeout] sets. *)

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
(** [copy_in db ~table ~columns row rows] writes the rows with one
    [COPY ... FROM STDIN], each bound as {!run} binds a statement's parameters,
    and answers how many were written. The sequence is read as the rows are
    sent, so a load of any size is held a row at a time.

    [columns] are in the row's order, every column in table order where empty,
    and quoted as given, so ["Jobs"] is not [jobs]; a number of them that is not
    the row's arity is refused before anything is sent. A row that cannot be
    bound, a constraint and a raise from the sequence each fail the whole COPY,
    with nothing written and the connection usable; the raise propagates.
    Cancellation closes the connection and cancels the COPY, as for any
    statement. For a few rows, an [insert] of [unnest]ed arrays is one statement
    too. *)

val connect :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  mono_clock:_ Eio.Time.Mono.t ->
  ?parameters:(string * string) list ->
  ?timeout:Mtime.Span.t ->
  ?statement_cache:int ->
  ?observe:observer ->
  Conninfo.t ->
  (conn, [> Rowtype.error ]) result
(** One connection, as [Postgres_eio.connect] makes it, every option passed
    through: a parameter setting one of the styles the driver reads text in
    another way is refused, as the driver refuses it. [observe] is around each
    statement it runs, a transaction's [begin] and [commit] among them, and
    nothing unless given. *)

val close : conn -> unit
(** Close it for good, telling the server; its next statement says so, and
    neither {!revive} nor a {!Transaction.within} opens it again. A no-op on one
    already closed. *)

val revive : conn -> (unit, [> Rowtype.error ]) result
(** Make again a connection a failure closed -- the server restarted, say -- in
    place, with its parameters; [Ok] at once for an open one. One {!close}
    closed is an [Error] and stays closed, and so is one that cannot be made
    again, saying why -- an address that does not answer, a password the server
    now refuses. *)

val timeout : conn -> Mtime.Span.t option
(** The bound on every read and write, [None] for none. *)

val set_timeout : conn -> timeout:Mtime.Span.t option -> unit
(** Set {!val-timeout}: lifted around work that runs longer, a migration, and
    put back after it. *)

(** Connections made up front and lent one at a time, as [Postgres_eio.Pool]
    lends them. *)
module Pool : sig
  type t

  val create :
    sw:Eio.Switch.t ->
    net:_ Eio.Net.t ->
    mono_clock:_ Eio.Time.Mono.t ->
    ?parameters:(string * string) list ->
    ?timeout:Mtime.Span.t ->
    ?statement_cache:int ->
    ?size:int ->
    ?wait:Mtime.Span.t ->
    ?reset:bool ->
    ?max_lifetime:Mtime.Span.t ->
    ?idle_check:Mtime.Span.t ->
    ?observe:observer ->
    Conninfo.t ->
    (t, [> Rowtype.error ]) result
  (** As [Postgres_eio.Pool.create], every option passed through; [observe] is
      {!connect}'s, for every connection it lends. *)

  type error =
    [ `Pool of
      [ `Busy  (** no connection came free within the wait *)
      | `Closed  (** the pool is closed *) ] ]
  (** What a borrow meets in the pool, a state of the pool's own. *)

  val use : ?wait:Mtime.Span.t -> t -> (conn -> 'a) -> ('a, [> error ]) result
  (** As [Postgres_eio.Pool.use]: [`Pool `Busy] if no connection came free
      within the wait, and [`Pool `Closed] at once if the pool is closed, or
      closes while the borrow waits. *)

  type stats = Postgres_eio.Pool.stats = {
    size : int;
    idle : int;
    waiting : int;
    replaced : int;
  }

  val stats : t -> stats
  (** As [Postgres_eio.Pool.stats]. *)

  val close : t -> unit
  (** As [Postgres_eio.Pool.close]. *)
end

(** Notifications, on a connection of their own, as [Postgres_eio.Listener]
    hears them: a session that ran [LISTEN] keeps what it hears, so a listener
    is never a pooled connection. *)
module Listener : sig
  type t

  type event =
    | Notification of { channel : string; payload : string }
    | Reconnected
        (** the connection was lost and made again, every channel listened to
            again: whatever was sent in between is gone *)

  val connect :
    sw:Eio.Switch.t ->
    net:_ Eio.Net.t ->
    mono_clock:_ Eio.Time.Mono.t ->
    ?parameters:(string * string) list ->
    ?timeout:Mtime.Span.t ->
    ?keep_alive:Mtime.Span.t ->
    Conninfo.t ->
    (t, [> Rowtype.error ]) result
  (** A connection that only listens, made as [Postgres_eio.Listener.connect]
      makes one, [parameters] and [timeout] passed through; [keep_alive] is how
      long {!next} waits in silence before it pings the server, 10 s unless
      given. *)

  val listen : t -> string -> (unit, [> Rowtype.error ]) result
  (** [LISTEN] on a channel named exactly as given. *)

  val unlisten : t -> string -> (unit, [> Rowtype.error ]) result
  (** [UNLISTEN] a channel, named as {!listen} named it: nothing more is heard
      on it, and a reconnection does not listen to it again. *)

  val next : t -> (event, [> Rowtype.error ]) result
  (** The next notification, or a wait for one; a lost connection is made again
      and said so. After {!close}, an error. *)

  val close : t -> unit
  (** Close it for good; {!next} then answers an error. *)
end

(** One transaction on one connection, and its own failures told apart from the
    work's.

    The work answers a [result]: [Ok] commits and [Error] rolls back. A failure
    of the transaction itself joins the work's error type as an open polymorphic
    variant -- the way [Eio.Time.with_timeout] adds [`Timeout] -- so a caller
    whose errors are polymorphic variants, as every statement's are, writes
    nothing, and one whose errors are ordinary variants maps once, in its own
    bracket. *)
module Transaction : sig
  (** How much of what concurrent transactions commit this one sees, as the SQL
      standard names the levels: Postgres's [begin isolation level]. *)
  type isolation = Read_committed | Repeatable_read | Serializable

  type failure =
    [ `Not_committed of string
      (** the transaction could not begin, its [COMMIT] failed, or the work
          answered [Ok] from a transaction a failed statement had aborted: in
          words for the log. Whether it committed is known: it did not. *)
    | `Conflict of string option
      (** its [COMMIT] found a deferred constraint refusing the write because of
          other rows -- a unique key, an exclusion, a foreign key, declared
          [DEFERRABLE] and checked there -- named as a statement's
          {!Rowtype.error} names one checked at once, and the server rolled it
          back *)
    | `Not_serializable of string
      (** its [COMMIT] found it could not be ordered with a concurrent
          transaction, as a statement's {!Rowtype.error} says when one does, and
          its retries did not get past it *)
    | `Lost of string
      (** its [COMMIT] was sent and the connection failed before the answer
          came: it may have committed, as a statement's {!Rowtype.error} says of
          a write lost in flight *)
    | `Unsent of string
      (** it never began: its connection, lost before it, could not be made
          again -- nothing answered -- so nothing of the work was sent, and
          running it again is safe *) ]
  (** The transaction's own failures, which {!within} adds to the work's: a
      caller tells them from its own as [#Transaction.failure], and names them
      in an error type of its own as [[ my_error | Transaction.failure ]]. *)

  val within :
    ?keep:('e -> bool) ->
    ?isolation:isolation ->
    ?retries:int ->
    conn ->
    (conn -> ('a, ([> failure ] as 'e)) result) ->
    ('a, 'e) result
  (** [within db work]: revive a connection the server dropped, begin -- once
      more if the first [begin] fails, the one point where starting again loses
      nothing -- run [work], and commit what it answered [Ok]. A refusal rolls
      back unless [keep] says to keep what it wrote -- a wrong sign-in code that
      has to count its attempt -- and still answers the refusal: keeping what a
      refused request wrote is the rarer thing, and the one a reader should see
      named. [keep] keeps nothing unless given.

      [isolation] is the level it begins at; unless given, the database's own
      default, which is [Read_committed] in a Postgres nobody configured.

      [retries] is how many times the whole transaction runs again when the work
      answers [`Not_serializable], or its [COMMIT] does -- none unless given, so
      a retry is always asked for. Each is an [info] line, and the last answer
      stands. Work that runs again does everything again, so work given retries
      does nothing but its statements: a message sent or a counter moved inside
      it happens once for every attempt.

      [`Unsent] replaces the work's answer when the transaction could not begin
      on a connection {!revive} could not reach. [`Not_committed] replaces it
      when the transaction could not begin otherwise -- on a connection the
      server refused to make again, with its reason --, when the server refused
      its [COMMIT] for anything but a conflict, or when the work answered [Ok]
      in a transaction a failed statement had already aborted -- an error the
      work swallowed. [`Conflict] replaces it when a deferred constraint refused
      the write at [COMMIT], as it would answer from the statement had the
      constraint been checked there. [`Lost] replaces it when the connection
      failed with [COMMIT] in flight, since what the server did is not known; a
      statement of the work lost before it is the work's own answer, and did
      nothing, as the transaction ended with its connection. A refusal from an
      aborted transaction is the work's own answer and is kept, and a kept
      refusal there keeps nothing it wrote, which a [warn] line says. A
      transaction already open on the connection -- a [within] inside another --
      is [`Not_committed] before anything begins, since its [COMMIT] would end
      the outer one, and the outer one goes on. Each [`Not_committed] and
      [`Lost] is an [error] line on [rowtype-postgres]. A raise inside [work]
      rolls back and passes.

      A fiber cancelled inside [work], at a statement or at any other effect,
      closes the connection, which ends the transaction with nothing kept, and
      the cancellation passes. Its statements then answer [`Connection `Closed],
      and the next [within] on it, or {!revive}, makes it again; a pooled one is
      made again before it is next lent. *)
end

(** {1 Statements against the database} *)

val verify :
  conn ->
  Rowtype.any list ->
  (unit, [> `Disagreements of string list | Rowtype.error ]) result
(** Every statement described by the database without being run, and what it
    says of each compared with what the statement declares: how many parameters
    and, for a statement that reads rows, how many columns, and each one's type
    -- an [int] an integer or an [oid], a [float] a float, a [text] text, a
    name, a uuid or JSON, and the rest their own, a domain as the type it is
    made from. An enum is read and written by a {!Rowtype.enum} each of whose
    labels is one of the enum's, and refused where its labels are none or repeat
    one; a label only the database has is no disagreement, since a migration
    adds one ahead of the code that reads it, and a row holding it is refused
    where it is read. An enum is also written from a [text], and read as one
    through a cast, [::text]. A statement this check accepts is one {!run}
    reads, in binary and in text alike. A parameter the database could not type
    is left to it. [`Disagreements] is every problem, in words for a person,
    each naming its statement's first line and where in it. Where the check
    could not ask -- a connection lost -- the answer is that failure, as a
    statement's is, and no statement is said to disagree.

    Whether a column may be NULL is not checked: the database says so only of a
    column read as it is stored, and a query may promise more, so a check would
    refuse what is right. A NULL a shape does not allow fails where it is read,
    naming its column. *)

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

(** {{!Rowtype}rowtype} over Postgres, through [postgres-eio]: the one place the
    two meet.

    Statements are Postgres's as written, [$1] for a shape's first value. An
    {!Rowtype.instant} is a [timestamptz], which keeps the microsecond too, sent
    as ISO 8601 in UTC and read back whatever the session's time zone, so a
    connection is asked for [DateStyle=ISO]. Years 1 to 9999: within a day of
    either end, a session far from UTC prints a year that a connection reading
    text cannot read. A [COMMIT] of a transaction a failed statement aborted is
    rolled back without an error, and {!commit} answers [`Rolled_back]. A result
    is read in binary wherever the driver reads its type, and in text on a
    connection that keeps no statements, which cannot be bound for binary. A
    unique, exclusion or foreign-key violation is [`Conflict], with the
    constraint the server named.

    Each statement is a [debug] line on [rowtype-postgres] with how long it
    took: its first line of text, never its parameters. A statement runs on the
    calling fiber, as an Eio effect, and a connection is one fiber's at a time.
*)

module Conninfo = Postgres_eio.Conninfo
(** Connection strings, both forms. *)

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
    Cancellation closes the connection. For a few rows, an [insert] of
    [unnest]ed arrays is one statement too. *)

val conninfo : string -> (Conninfo.t, [> Rowtype.error ]) result
(** A connection string read, or what in it could not be. *)

val on_database : server:string -> string -> (string, [> Rowtype.error ]) result
(** [on_database ~server name]: the connection string [server], naming the
    database [name] instead, as a URL -- the server's own [postgres] to make or
    drop a database, or a database of a test's own. *)

val connect :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  mono_clock:_ Eio.Time.Mono.t ->
  ?parameters:(string * string) list ->
  ?timeout_s:float ->
  ?statement_cache:int ->
  ?observe:observer ->
  Conninfo.t ->
  (conn, [> Rowtype.error ]) result
(** One connection, as [Postgres_eio.connect] makes it, every option passed
    through; [observe] around each statement it runs, a transaction's [begin]
    and [commit] among them, and nothing unless given. *)

val close : conn -> unit

val revive : conn -> unit
(** Make again a connection a failure closed -- the server restarted, say -- in
    place, with its parameters. A no-op on an open one; one that cannot be made
    again stays closed, and its next statement says so. *)

val timeout : conn -> float option

val set_timeout : conn -> float option -> unit
(** The bound on every read and write, [None] for none: lifted around work that
    runs longer, a migration. *)

(** Connections made up front and lent one at a time, as [Postgres_eio.Pool]
    lends them. *)
module Pool : sig
  type t

  val create :
    sw:Eio.Switch.t ->
    net:_ Eio.Net.t ->
    mono_clock:_ Eio.Time.Mono.t ->
    ?parameters:(string * string) list ->
    ?timeout_s:float ->
    ?statement_cache:int ->
    ?size:int ->
    ?wait_s:float ->
    ?reset:bool ->
    ?max_lifetime_s:float ->
    ?idle_check_s:float ->
    ?observe:observer ->
    Conninfo.t ->
    (t, [> Rowtype.error ]) result
  (** As [Postgres_eio.Pool.create], every option passed through; [observe] is
      {!connect}'s, for every connection it lends. *)

  val use :
    ?wait_s:float -> t -> (conn -> 'a) -> ('a, [> `Busy of float ]) result
  (** As [Postgres_eio.Pool.use]. *)

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
    ?timeout_s:float ->
    ?heartbeat_s:float ->
    Conninfo.t ->
    (t, [> Rowtype.error ]) result

  val listen : t -> string -> (unit, [> Rowtype.error ]) result
  (** [LISTEN] on a channel named exactly as given. *)

  val next : t -> (event, [> Rowtype.error ]) result
  (** The next notification, or a wait for one; a lost connection is made again
      and said so. After {!close}, an error. *)

  val close : t -> unit
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
  type failure =
    [ `Busy of float  (** no connection within the wait, in seconds *)
    | `Not_committed of string
      (** the transaction could not begin, its [COMMIT] failed, or the work
          returned [Ok] from a transaction a failed statement had aborted: in
          words for the log *)
    | `Not_serializable of string
      (** its [COMMIT] found it could not be ordered with a concurrent
          transaction, as a statement's {!Rowtype.error} says when one does *)
    ]

  (** How much of what concurrent transactions commit this one sees, as the SQL
      standard names the levels: Postgres's [begin isolation level]. *)
  type isolation = Read_committed | Repeatable_read | Serializable

  val within :
    ?keep:('e -> bool) ->
    ?isolation:isolation ->
    ?retries:int ->
    conn ->
    (conn ->
    ( 'a,
      ([> `Not_committed of string | `Not_serializable of string ] as 'e) )
    result) ->
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

      [`Not_committed] replaces the work's answer when the transaction could not
      begin, when [COMMIT] failed, or when the work answered [Ok] in a
      transaction a failed statement had already aborted -- an error the work
      swallowed. A refusal from an aborted transaction is the work's own answer
      and is kept, and a kept refusal there keeps nothing it wrote, which a
      [warn] line says. A transaction already open on the connection -- a
      [within] inside another -- is [`Not_committed] before anything begins,
      since its [COMMIT] would end the outer one, and the outer one goes on.
      Each [`Not_committed] is an [error] line on [rowtype-postgres]. A raise
      inside [work] rolls back and passes. *)
end

(** {1 Statements against the database} *)

val verify : conn -> Rowtype.any list -> (unit, string list) result
(** Every statement described by the database without being run, and what it
    says of each compared with what the statement declares: how many parameters
    and, for a statement that reads rows, how many columns, and each one's type
    -- an [int] an integer or an [oid], a [float] a float, a [text] text, a
    name, a uuid, JSON or an enum's label, and the rest their own, a domain as
    the type it is made from. A parameter the database could not type is left to
    it. [Error] is every problem, each naming its statement's first line and
    where in it; or, where the check could not ask -- a connection lost -- that
    alone.

    Whether a column may be NULL is not checked: the database says so only of a
    column read as it is stored, and a query may promise more, so a check would
    refuse what is right. A NULL a shape does not allow fails where it is read,
    naming its column. *)

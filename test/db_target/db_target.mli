(** Where a suite's database comes from: the server its runner names, and a
    database of a case's own on it. *)

(** {1 The server} *)

val variable : string
(** [ROWTYPE_TEST_PG], the variable a suite reads the server's URL from where it
    starts. *)

val skipped : suite:string -> string
(** The line a suite prints when {!variable} is unset, saying it is skipped and
    why. *)

val eio : Eio_unix.Stdenv.base -> sw:Eio.Switch.t -> server:string -> unit
(** [eio env ~sw ~server] hands the suite's Eio loop, a switch that lasts as
    long and the server's URL over once, before any case runs. *)

(** {1 What a case reaches for} *)

val env : unit -> Eio_unix.Stdenv.base
val sw : unit -> Eio.Switch.t
val net : unit -> [ `Generic | `Unix ] Eio.Net.ty Eio.Net.t
val mono : unit -> Eio.Time.Mono.ty Eio.Time.Mono.t

val on_database : string -> Rowtype_postgres.Conninfo.t
(** The server, naming the database named. *)

val conninfo : string -> Rowtype_postgres.Conninfo.t
(** A connection string read, or the case fails. *)

val connect :
  ?statement_cache:int -> Rowtype_postgres.Conninfo.t -> Rowtype_postgres.conn
(** A connection, or the case fails. [~statement_cache:0] is one that keeps no
    statements, whose results all arrive as text. *)

val admin : (Rowtype_postgres.conn -> 'a) -> 'a
(** [admin f] is [f] over a connection to the server's own database, closed
    after. *)

val exec : Rowtype_postgres.conn -> string -> unit
(** Runs a statement, or the case fails naming it. *)

val with_postgres : (Rowtype_postgres.Conninfo.t -> 'a) -> 'a
(** [with_postgres f] is [f] over a database of its own, made for it and dropped
    after. *)

(** {1 Shapes the cases write in} *)

val instant_us : int Rowtype.ty
(** An instant as epoch microseconds. *)

val timestamp_us : int Rowtype.ty
(** A timestamp as epoch microseconds. *)

val uuid_text : string Rowtype.ty
(** A uuid as its text, lowercase. *)

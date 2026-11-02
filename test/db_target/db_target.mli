(** Where a suite's database comes from: the server its runner names, and a
    database of a case's own on it. *)

(** {1 The server} *)

type t
(** A suite's server, with the Eio loop and switch its cases run on: made once
    by {!start}, and handed to each case that needs it. *)

val variable : string
(** [TEST_DATABASE_URL], the variable a suite reads a server's URL from where it
    starts, where its runner hands one. *)

val start :
  Eio_unix.Stdenv.base ->
  sw:Eio.Switch.t ->
  ?url:string ->
  unit ->
  (t, [> Rowtype_test.error | Rowtype.error ]) result
(** [start env ~sw ?url ()] is the server at [url], or one of the suite's own
    ({!Rowtype_test.server}), removed as [sw] ends, with an empty template each
    case's database is copied from. *)

val pg_dump : t -> string option
(** The command that dumps a database, [{database}] its name, where the server
    is the suite's own: [pg_dump] run in its container. *)

(** {1 What a case reaches for} *)

val env : t -> Eio_unix.Stdenv.base
val sw : t -> Eio.Switch.t
val net : t -> [ `Generic | `Unix ] Eio.Net.ty Eio.Net.t
val mono : t -> Eio.Time.Mono.ty Eio.Time.Mono.t

val on_database : t -> string -> Rowtype_postgres.Conninfo.t
(** The server, naming the database named. *)

val conninfo : string -> Rowtype_postgres.Conninfo.t
(** A connection string read, or the case fails. *)

val connect :
  t ->
  ?statement_cache:int ->
  Rowtype_postgres.Conninfo.t ->
  Rowtype_postgres.conn
(** A connection on the suite's switch, or the case fails. [~statement_cache:0]
    is one that keeps no statements, whose results all arrive as text. *)

val admin : t -> (Rowtype_postgres.conn -> 'a) -> 'a
(** [admin db f] is [f] over a connection to the server's own database, closed
    after. *)

val exec : Rowtype_postgres.conn -> string -> unit
(** Runs a statement, or the case fails naming it. *)

val with_postgres : t -> (Rowtype_postgres.Conninfo.t -> 'a) -> 'a
(** [with_postgres db f] is [f] over a database of its own, made for it and
    dropped after. *)

(** {1 Shapes the cases write in} *)

val instant_us : int Rowtype.ty
(** An instant as epoch microseconds. *)

val timestamp_us : int Rowtype.ty
(** A timestamp as epoch microseconds. *)

val uuid_text : string Rowtype.ty
(** A uuid as its text, lowercase. *)

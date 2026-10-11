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

val sw : unit -> Eio.Switch.t
val net : unit -> [ `Generic | `Unix ] Eio.Net.ty Eio.Net.t
val mono : unit -> Eio.Time.Mono.ty Eio.Time.Mono.t
val process_mgr : unit -> Eio_unix.Process.mgr_ty Eio.Process.mgr

val on_database : string -> string
(** The server's URL, with the database path replaced by the one named. *)

val connect : ?statement_cache:int -> string -> Rowtype_postgres.conn
(** A connection to a URL, or the case fails. [~statement_cache:0] is one that
    keeps no statements, whose results all arrive as text. *)

val admin : (Rowtype_postgres.conn -> 'a) -> 'a
(** [admin f] is [f] over a connection to the server's own database, closed
    after. *)

val exec : Rowtype_postgres.conn -> string -> unit
(** Runs a statement, or the case fails naming it. *)

val with_postgres : (string -> 'a) -> 'a
(** [with_postgres f] is [f] over the URL of a database of its own, made for it
    and dropped after. *)

(** {1 Shapes the cases write in} *)

val instant_us : int Rowtype.ty
(** An instant as epoch microseconds. *)

val timestamp_us : int Rowtype.ty
(** A timestamp as epoch microseconds. *)

val uuid_text : string Rowtype.ty
(** A uuid as its text, lowercase. *)

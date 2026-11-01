(** A Postgres database a test run owns, as Testcontainers gives one in other
    languages: a server for the run, a database of its own in it for each case
    that asks, and nothing left once the run ends -- whether it passed, failed
    or raised.

    {[
    let () =
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      match Rowtype_test.server ~sw ?url:(Sys.getenv_opt "TEST_DATABASE_URL") env with
      | Error e -> failwith (Rowtype_test.error_to_string e)
      | Ok server ->
          Rowtype_test.database server (fun conninfo -> ...)
    ]}

    Handed a server's [url], it starts nothing, which is how a test runs where
    there is no container runtime -- inside HAF's walled gates, which hand one.
    Handed none, it starts Postgres in Podman, by its command line: its data in
    memory, its durability off, on a port the system chooses, labelled with the
    process that started it; and it removes the container when [sw] ends. A
    container a run left when it was killed is removed by the next run's
    {!val-server}, so the next run is the reaper. *)

type server
(** A Postgres server a run makes its databases in. *)

type error =
  [ `Runtime of string
    (** Podman could not start, read or remove the container, in its words *)
  | `Not_ready of string
    (** Postgres did not answer within a minute, saying the last failure *)
  | `Unreadable of string  (** the [url] handed is no connection string *) ]

val error_to_string : [< error ] -> string

val server :
  sw:Eio.Switch.t ->
  ?url:string ->
  ?image:string ->
  < process_mgr : _ Eio.Process.mgr
  ; net : _ Eio.Net.t
  ; mono_clock : _ Eio.Time.Mono.t
  ; .. > ->
  (server, [> error ]) result
(** The server at [url], or a container of [image] -- Postgres 18 on Alpine
    unless given -- started, waited on until it answers, and removed when [sw]
    ends. Before it starts one, every container a run of this library started
    whose process has ended is removed. *)

val conninfo : server -> Rowtype_postgres.Conninfo.t
(** The server's own database, to connect to as its owner. *)

val container : server -> string option
(** The container's name, where it was started here: for a command run in it,
    [pg_dump] say, as its owner, the user [test]. [None] for a server handed by
    [url]. *)

val database :
  server ->
  (Rowtype_postgres.Conninfo.t -> 'a) ->
  ('a, [> Rowtype.error ]) result
(** [database server f] is [f] over a database of its own, made for it in the
    server and dropped after, even where [f] raises, and dropped with [(force)],
    so a connection [f] left open does not keep it. *)

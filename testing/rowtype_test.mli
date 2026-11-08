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
    Handed none, it starts Postgres in Podman or Docker, by its command line:
    its data in memory, its durability off, on a port the system chooses,
    labelled with the process that started it; and it removes the container when
    [sw] ends. A container a run left when it was killed is removed by the next
    run's {!val-server}, so the next run is the reaper.

    Its databases are named at random, so runs in containers of their own --
    where every process may have the same id -- share a server handed to them at
    once. What a killed run left on a handed server stays, its owner's to
    remove, and blocks no other run.

    What a cleanup could not remove, where no caller is left to answer -- a
    container, a template, a case's database its case raised past -- is a
    warning on the [Logs] source [rowtype-test], naming it. *)

type server
(** A Postgres server a run makes its databases in. *)

type runtime = [ `Podman | `Docker ]
(** The container runtime a server is started in, by its command line: [podman]
    or [docker]. *)

val container_to_string : runtime * string -> string
(** A container in words a runner hands on -- [podman rowtype-test-1-2], its
    runtime's binary and its name -- as [rowtype-test run] hands its own. *)

val container_of_string : string -> (runtime * string) option
(** A container from {!container_to_string}'s words; [None] for any other. *)

type error =
  [ `Runtime of runtime * string
    (** the runtime could not be run, or could not start, read or remove the
        container, in its words *)
  | `Not_ready of string
    (** Postgres did not answer within a minute, saying the last failure *)
  | `Unreadable of string  (** the [url] handed is no connection string *) ]

val error_to_string : [< error | Rowtype.error ] -> string
(** In words for a log; a database's failure as {!Rowtype.error_to_string} words
    it. *)

val server :
  sw:Eio.Switch.t ->
  ?url:string ->
  ?container:runtime * string ->
  ?runtime:runtime ->
  ?image:string ->
  < process_mgr : _ Eio.Process.mgr
  ; net : _ Eio.Net.t
  ; mono_clock : _ Eio.Time.Mono.t
  ; .. > ->
  (server, [> error ]) result
(** The server at [url], or a container of [image] -- Postgres 18 on Alpine
    unless given -- started in [runtime] -- Podman unless given -- waited on
    until it answers, and removed when [sw] ends. Before it starts one, every
    container a run of this library started whose process has ended is removed.
    [container], with [url], is the container that server runs in, where its
    runner says so: {!val-container} answers it, for a command run there.
    Without [url], it is not read. *)

val conninfo : server -> Rowtype_postgres.Conninfo.t
(** The server's own database, to connect to as its owner. *)

val container : server -> (runtime * string) option
(** The runtime and the container's name, where it was started here: for a
    command run in it, [pg_dump] say, as its owner, the user [test]. [None] for
    a server handed by [url]. *)

type template
(** A database a suite's setup made once -- its migrations run, say -- that each
    case is handed a copy of. *)

val template :
  ?setup:
    (Rowtype_postgres.Conninfo.t ->
    (unit, ([> error | Rowtype.error ] as 'e)) result) ->
  server ->
  (template, 'e) result
(** [template server ~setup] makes a database in the server, runs [setup] in it
    once, ends every connection to it -- one [setup] left open included -- and
    marks it a template no one may connect to: what every {!database} made from
    it is a copy of, rows the setup wrote included. With no [setup], an empty
    database. A [setup] that fails is the template's failure as itself, its tags
    merged with these, so a migration's is matched as the migration words it. It
    is dropped when the server's switch ends. Making it needs a user allowed to
    create databases, which the container's owner is. *)

val database :
  template ->
  (Rowtype_postgres.Conninfo.t -> 'a) ->
  ('a, [> Rowtype.error ]) result
(** [database template f] is [f] over a copy of [template] of its own --
    [CREATE DATABASE ... TEMPLATE], tens of milliseconds -- dropped after, even
    where [f] raises or its fiber is cancelled, and dropped with [(force)], so a
    connection [f] left open does not keep it. A copy that could not be dropped
    is the [Error], where [f] answered; where [f] raised, the raise passes and
    the copy is a warning. *)

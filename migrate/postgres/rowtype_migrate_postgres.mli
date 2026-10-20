(** Migrations on Postgres, by {!Rowtype_migrate}'s files and rules: the library
    [rowtype-migrate.postgres], built where [rowtype-postgres] is installed.

    A run holds a Postgres advisory lock, so two processes migrating at once
    take turns and the second finds nothing left to do; each migration runs only
    on the session that took it. The connection's bounds are lifted for the run:
    a migration is bounded by nothing but itself. A transaction is known by its
    id, so a file that ends its own -- and begins another after it -- is found.
    What a migration sets for its session ends with it, the session's parameters
    reset after each ([reset all]): a [set search_path], a [set lock_timeout],
    the empty search path a baseline's pg_dump starts with. A Postgres record
    made before its digests or its sources has them added on its next run, every
    row the application's. *)

val run :
  < net : _ Eio.Net.t ; mono_clock : _ Eio.Time.Mono.t ; .. > ->
  Rowtype_postgres.Conninfo.t ->
  (Rowtype_postgres.conn -> ('a, ([> Rowtype_migrate.error ] as 'e)) result) ->
  ('a, 'e) result
(** [run env target f]: a connection to the database, as the commands make one,
    on a switch of its own and with the network and the monotonic clock taken
    from [env], handed to [f] and closed when [f] returns or raises:

    {[
    Rowtype_migrate_postgres.run env target @@ fun db ->
    Rowtype_migrate_postgres.apply db migrations
    ]}

    Its connect bounded at ten seconds unless the target sets its
    [connect_timeout], and nothing after that, since an administrator's
    statement runs as long as it takes; notices off, since
    [create table if not exists] is one on every run. A database the server does
    not have is [`No_database]; any other failure to connect is the driver's. *)

val create :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  mono_clock:_ Eio.Time.Mono.t ->
  Rowtype_postgres.Conninfo.t ->
  (unit, [> Rowtype_migrate.error ]) result
(** [create ~sw ~net ~mono_clock target]: make the database the target names,
    connecting as {!run} does but on [sw], from the server's own [postgres]; one
    that exists is refused, as Postgres refuses it. *)

val drop :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  mono_clock:_ Eio.Time.Mono.t ->
  Rowtype_postgres.Conninfo.t ->
  (unit, [> Rowtype_migrate.error ]) result
(** [drop ~sw ~net ~mono_clock target]: drop the database the target names, if
    it is there, connecting as {!run} does but on [sw]. Refused while anybody is
    connected to it, as Postgres refuses it: the one guard there is against the
    wrong URL. *)

val default_lock : int
(** The advisory lock {!apply} takes unless told another, so any two processes
    migrating one database take turns. *)

val apply :
  ?lock:int ->
  ?table:string ->
  ?libraries:Rowtype_migrate.source list ->
  Rowtype_postgres.conn ->
  Rowtype_migrate.migration list ->
  (unit, [> Rowtype_migrate.error ]) result
(** Apply every migration in the list that the database has not recorded, as
    {!Rowtype_migrate.Make}'s [apply] does, under the advisory lock [lock], any
    number fixed for the database. A connection lost between two migrations,
    which a transaction would make again on a new backend holding no lock, stops
    the run as [`Lock `Lost], the ones before it applied and nothing more. *)

val status :
  ?table:string ->
  ?libraries:Rowtype_migrate.source list ->
  Rowtype_postgres.conn ->
  Rowtype_migrate.migration list ->
  (Rowtype_migrate.status list, [> Rowtype_migrate.error ]) result
(** What {!apply} would do, without doing it. *)

val dump :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  mono_clock:_ Eio.Time.Mono.t ->
  process_mgr:_ Eio.Process.mgr ->
  ?lock:int ->
  ?table:string ->
  ?libraries:Rowtype_migrate.source list ->
  ?pg_dump:string ->
  ?restrict_key:string ->
  migrations:Rowtype_migrate.migration list ->
  Rowtype_postgres.Conninfo.t ->
  (string, [> Rowtype_migrate.error ]) result
(** What the migrations add up to from nothing, the libraries' with the
    application's: a database of its own on the server the target names,
    migrated, dumped, and dropped however that went -- never what some database
    has drifted to.

    [pg_dump] is the command, ["pg_dump --dbname={url}"] unless given, split on
    spaces, with [{url}] and [{database}] replaced in each word by the scratch
    database's; it is not given to a shell, so a word cannot hold a space. It
    runs through [process_mgr], so a cancelled dump kills it, and what it writes
    as errors is [`Dump]'s words rather than lines on the caller's terminal. Its
    connections are made as {!run} makes one, on [sw]. It dumps the schema
    alone, with [restrict_key], ["rowtype"] unless given, as the key a dump
    otherwise makes up afresh each time, and the lines naming the versions of
    the server and of [pg_dump] are removed, so the file changes only when the
    schema does. *)

val squash :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  mono_clock:_ Eio.Time.Mono.t ->
  process_mgr:_ Eio.Process.mgr ->
  ?lock:int ->
  ?table:string ->
  ?libraries:Rowtype_migrate.source list ->
  ?pg_dump:string ->
  ?restrict_key:string ->
  through:Rowtype_migrate.version ->
  migrations:Rowtype_migrate.migration list ->
  Rowtype_postgres.Conninfo.t ->
  (Rowtype_migrate.migration, [> Rowtype_migrate.error ]) result
(** Every migration through the version [through] names, as one baseline, as
    {!Rowtype_migrate.squash} proves one: a database of its own on the server
    the target names, migrated through it and dumped by [pg_dump], as {!dump}'s
    is -- the schema and the rows the migrations wrote, as statements, and never
    the record. Its version is [through], its name [baseline]. With [libraries],
    they run first on each scratch database, and the tables they made are left
    out of the baseline; a library that made anything but tables -- a type, a
    function, a view -- is [`Invalid_argument]. *)

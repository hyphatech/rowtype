(** Migrations for Postgres: plain SQL files, forward only, each applied in one
    transaction with the row that records it unless its first line says not.

    A migration is a file named [<version>_<name>.sql] in one directory, the
    version a UTC timestamp of fourteen digits. They are applied by the
    [rowtype-migrate] command, which links no application:

    {v
    rowtype-migrate up                    apply what is pending
    rowtype-migrate status [--check]      applied and pending, by version
    rowtype-migrate new add_users         migrations/<UTC stamp>_add_users.sql
    rowtype-migrate dump [--check]        what the migrations add up to, as one file
    rowtype-migrate squash VERSION        every migration through it, as one
    rowtype-migrate create                make the database the URL names
    rowtype-migrate drop                  drop it
    v}

    The database is [--url], or else the variable [--env] names --
    [DATABASE_URL] unless given -- read from [--env-file] if one is given and
    from the environment if not; the files are [--dir], [migrations] unless
    given; the record is [--table], {!default_table} unless given. Every command
    has [--help].

    An application never migrates, and knows nothing of its migrations: they are
    applied as a step of their own before it starts, and [status --check] is how
    a script asks whether every one is. {!run} is for a test and a tool. The way
    back from a bad migration is the next one.

    A file runs in a transaction with the row that records it, unless its first
    line is the directive

    {v -- rowtype-migrate: no transaction v}

    for the statements Postgres refuses inside one --
    [create index concurrently], [vacuum]. Such a file is one statement, since
    several sent together are a transaction of their own, and it is recorded
    after it succeeds: a run cut off between the two runs it again, so it is
    written to run twice ([create index concurrently if not exists]).

    A history grows, and a database made from nothing replays all of it; a
    {!squash} replaces every file through a version with one baseline, whose
    first line is [-- rowtype-migrate: baseline] and whose version is the last
    one it replaced. A database that recorded that version has the baseline
    already, and what it recorded up to it is history: accepted, and compared
    with nothing. A database made afterwards runs the baseline. One whose record
    stops before it is refused: it is migrated with a build from before the
    squash first. *)

(** How a migration is applied. *)
type kind =
  | Transaction  (** in one transaction with the row that records it *)
  | No_transaction
      (** on its own, and recorded after it: the file's first line is
          [-- rowtype-migrate: no transaction] *)
  | Baseline
      (** every migration through its version, as one, in a transaction: the
          file's first line is [-- rowtype-migrate: baseline], and a list holds
          one, the oldest *)

type version = private int
(** A UTC timestamp, [YYYYMMDDHHMMSS], as its number -- a timestamp rather than
    a counter, because two branches that each add "migration 7" collide on merge
    where two timestamps do not. One a program makes has fourteen digits; one a
    database recorded is as it recorded it. *)

val version_of_int : int -> version option
(** [Some] for a number of fourteen digits. *)

type migration = {
  version : version;
  name : string;
  kind : kind;  (** read from the file's first line *)
  sql : string;
}

type error =
  [ Rowtype.error
  | `Files of string
    (** the migrations' files: one not named as a migration, one that cannot be
        read or written, a directive not known, a version twice, a baseline out
        of place -- in words naming it *)
  | `Invalid of string
    (** an argument outside its grammar -- a table's name, a migration's name --
        or a squash through no migration it can squash: in words *)
  | `Behind of migration
    (** the database's record stops before the list's baseline: it is migrated
        with a build from before the squash first *)
  | `Unknown of version
    (** the database records a version the list does not know: it was migrated
        from a newer checkout *)
  | `Edited of migration
    (** its text is not what the database applied: edited after it ran *)
  | `Late of migration
    (** pending, and older than one the database applied: merged after a later
        one ran *)
  | `Failed of migration * Rowtype.error
    (** it failed, and the database is at the version before it -- or, for one
        run outside a transaction, at whatever the failure left *)
  | `No_database of string
    (** the server has no database of this name: it is made with
        [rowtype-migrate create] *)
  | `Dump of string
    (** [pg_dump] could not run, or failed: in words naming the program and
        never its arguments, which hold the URL *)
  | `Unproved  (** a squash's baseline does not make what the history makes *)
  ]
(** Every way migrating fails: a polymorphic variant, as a statement's failures
    are, so it joins a caller's own. *)

val error_to_string : [< error ] -> string
(** In words for a person, naming the migration and what to do next. *)

(** {1 The files} *)

val of_files : string list -> (migration list, [> error ]) result
(** The files read, in version order. Refused, naming it: a file not named
    [<14-digit UTC timestamp>_<name>.sql], one that cannot be read, one whose
    first line is a directive this does not know, a version used twice -- two
    branches that each added a migration in the same second, a merge somebody
    has to look at -- and a baseline that is not the oldest, or not the only
    one. *)

val of_directory : string -> (migration list, [> error ]) result
(** Every [.sql] file in the directory, as {!of_files} reads them. *)

val new_migration :
  dir:string -> now:Ptime.t -> string -> (string, [> error ]) result
(** [new_migration ~dir ~now name] writes an empty migration named for [now], as
    a UTC stamp to the second, and answers its path, making [dir] if it is not
    there. A name is lower-case letters, digits and underscores; a stamp already
    in [dir] is refused, since two migrations may not share a version. *)

(** {1 A database} *)

val with_connection :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  mono_clock:_ Eio.Time.Mono.t ->
  string ->
  (Rowtype_postgres.conn -> ('a, ([> error ] as 'e)) result) ->
  ('a, 'e) result
(** A connection to the database the URL names, as the commands make one, for
    the length of the function: its connect bounded at ten seconds unless the
    URL says [connect_timeout], and nothing after that, since an administrator's
    statement runs as long as it takes; notices off, since
    [create table if not exists] is one on every run. A database the server does
    not have is [`No_database]; any other failure to connect is the driver's. *)

val create :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  mono_clock:_ Eio.Time.Mono.t ->
  string ->
  (unit, [> error ]) result
(** Make the database the URL names, from the server's own [postgres]; one that
    exists is refused, as Postgres refuses it. *)

val drop :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  mono_clock:_ Eio.Time.Mono.t ->
  string ->
  (unit, [> error ]) result
(** Drop the database the URL names, if it is there. Refused while anybody is
    connected to it, as Postgres refuses it: the one guard there is against the
    wrong URL. *)

val default_lock : int
(** The advisory lock {!run} takes unless told another, so any two processes
    migrating one database take turns. *)

val default_table : string
(** [schema_migrations], the table the database's record is kept in unless told
    another. Every function that reads or writes the record takes [?table]: a
    name of lower-case letters, digits and underscores, after a schema and a dot
    if it names one, found through the search path as the statements that write
    it find it -- so two applications can keep two records in one database. A
    name outside that grammar is refused, since a table's name cannot be a
    parameter. *)

val run :
  ?lock:int ->
  ?table:string ->
  Rowtype_postgres.conn ->
  migration list ->
  (unit, [> error ]) result
(** Apply every migration in the list that the database has not recorded, in
    version order, under a Postgres advisory lock -- so two processes migrating
    at once take turns, and the second finds nothing left to do. [lock] is any
    number fixed for the database. The connection's bounds are lifted for the
    run: a migration is bounded by nothing but itself.

    Refused, naming the migration, and nothing applied:
    - [`Behind], a database whose record stops before the list's baseline, which
      has to be migrated with a build from before the squash first;
    - [`Unknown], a database that records a version the list does not know,
      which was migrated from a newer checkout: applying an older one would run
      a history that is not the database's;
    - [`Edited], a migration whose text is not what the database applied --
      edited after it ran, where a schema that has run changes by the next
      migration;
    - [`Late], a pending migration older than one already applied, merged after
      a later one ran, which would run out of the order it was written in.

    A migration that fails is [`Failed], and the ones before it stay applied.

    Each applied migration is recorded with a digest of its text, and a database
    migrated before the digests were kept has them recorded on its next run.
    What a baseline replaced is history: recorded, and compared with nothing.

    A baseline is a pg_dump, which sets the session's parameters as it starts --
    an empty search path among them -- and so the session's parameters are reset
    after one, as a connection's own would be ([reset all]). *)

type status = {
  applied : (version * string) list;  (** version and name, as recorded *)
  pending : migration list;  (** in the list and not recorded, in order *)
  unknown : version list;
      (** recorded and not in the list: a newer checkout's *)
  edited : version list;  (** recorded, with a text that has changed since *)
  late : version list;  (** pending, and older than one already applied *)
  behind : version option;
      (** the list's baseline, where the database's record stops before it *)
}

val status :
  ?table:string ->
  Rowtype_postgres.conn ->
  migration list ->
  (status, [> error ]) result
(** What {!run} would do, without doing it; it writes nothing, not even the
    table that records migrations. *)

(** {1 The schema, as one file} *)

val dump :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  mono_clock:_ Eio.Time.Mono.t ->
  ?lock:int ->
  ?table:string ->
  pg_dump:string ->
  restrict_key:string ->
  migrations:migration list ->
  string ->
  (string, [> error ]) result
(** What the migrations add up to from nothing: a database of its own on the
    server the URL names, migrated, dumped, and dropped however that went --
    never what some database has drifted to.

    [pg_dump] is the command, split on spaces, with [{url}] and [{database}]
    replaced in each word by the scratch database's; it is not given to a shell,
    so a word cannot hold a space. It dumps the schema alone, with
    [restrict_key] as the key a dump otherwise makes up afresh each time, and
    the lines naming the versions of the server and of [pg_dump] are removed, so
    the file changes only when the schema does. *)

val squash :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  mono_clock:_ Eio.Time.Mono.t ->
  ?lock:int ->
  ?table:string ->
  pg_dump:string ->
  restrict_key:string ->
  through:version ->
  migrations:migration list ->
  string ->
  (migration, [> error ]) result
(** Every migration through the version [through] names, as one baseline: a
    database of its own on the server the URL names, migrated through it and
    dumped -- the schema and the rows the migrations wrote, as statements, and
    never the record -- by [pg_dump], as {!dump}'s is. Its version is [through],
    its name [baseline].

    It is proved before it is answered: the baseline and the migrations after it
    make a database that dumps the same, row for row, as the one the whole
    history makes, or it is [`Unproved]. Writing it and removing the files it
    replaced is the caller's. [`Invalid]: a [through] that is no migration's
    version, and one that is the baseline's with nothing before it. *)

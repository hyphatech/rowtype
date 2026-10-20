(** Migrations as plain SQL files, forward only, each applied in one transaction
    with the row that records it unless its first line says not: the files, the
    record and its rules, the same for every dialect. A database's own functions
    -- {!Rowtype_migrate_postgres}, {!Rowtype_migrate_sqlite} -- are each a
    library of their own, [rowtype-migrate.postgres] and
    [rowtype-migrate.sqlite], built where that dialect's backend is installed,
    so a program links the dialects it uses and no other.

    A migration is a file named [<version>_<name>.sql] in one directory, the
    version a UTC timestamp of fourteen digits. They are applied by the
    [rowtype-migrate] command, which links no application:

    {v
    rowtype-migrate up                    apply what is pending
    rowtype-migrate status [--check]      applied and pending, by version
    rowtype-migrate new add_users         migrations/<UTC stamp>_add_users.sql
    rowtype-migrate up --source spool     a library's migrations, then these
    rowtype-migrate dump [--check]        what the migrations add up to, as one file
    rowtype-migrate squash VERSION        every migration through it, as one
    rowtype-migrate create                make the database the URL names
    rowtype-migrate drop                  drop it
    v}

    The database is [--url] -- a Postgres URL or keyword list, or [sqlite:PATH]
    -- or else the variable [--env] names -- [DATABASE_URL] unless given -- read
    from [--env-file] if one is given and from the environment if not; the files
    are [--dir], [migrations] unless given; the record is [--table],
    {!default_table} unless given. Every command has [--help].

    A library that owns a table ships the migrations that make it, as files of
    its own installed with its package, in [share/<package>/migrations]; the
    command applies them beside the application's with [--source <package>], or
    [--source <name>=<dir>] for a folder anywhere. Each source is a history of
    its own, recorded with its name -- the application's is [app] -- and ordered
    and checked only among its own, so a library's migration is never late
    beside an application's. Libraries run first, in the order given, and the
    application last, since its migrations may stand on their tables.

    An application never migrates, and knows nothing of its migrations: they are
    applied as a step of their own before it starts, and [status --check] is how
    a script asks whether every one is. a dialect's [apply] is for a test and a
    tool. The way back from a bad migration is the next one.

    A file runs in a transaction with the row that records it, unless its first
    line is the directive

    {v -- rowtype-migrate: no transaction v}

    for the statements a database refuses inside one -- Postgres's
    [create index concurrently] and [vacuum], SQLite's [vacuum] and the
    [pragma foreign_keys = off] its table rebuild needs. Such a file is one
    statement, since several sent together are a transaction of their own, and
    it is recorded after it succeeds: a run cut off between the two runs it
    again, so it is written to run twice
    ([create index concurrently if not exists]).

    A file in a transaction that ends it itself -- its own [commit] or
    [rollback] -- is refused, whether or not what follows succeeds, and its
    record removed, since what ran after that end ran outside the transaction: a
    migration is applied as one, or says on its first line that it is not.

    A history grows, and a database made from nothing replays all of it; a
    {!squash} replaces every file through a version with one baseline, whose
    first line is [-- rowtype-migrate: baseline] and whose version is the last
    one it replaced. A database that recorded that version has the baseline
    already, and what it recorded below it is history: accepted, and compared
    with nothing, as is the migration at that version the baseline replaced. A
    database made afterwards runs the baseline, and is held to its text as to
    any migration's. One whose record stops before it is refused: it is migrated
    with a build from before the squash first. *)

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
  | `Invalid_argument of string
    (** an argument outside what it may be -- a table's, a migration's or a
        source's name outside its grammar, a source given twice, a squash
        through no migration it can squash: in words naming it *)
  | `Record of
    [ `Behind of migration
      (** it stops before the list's baseline: it is migrated with a build from
          before the squash first *)
    | `Unknown of version
      (** it holds a version the list does not know: it was migrated from a
          newer checkout *)
    | `Edited of migration
      (** it holds a digest of another text: the file was edited after it ran *)
    | `Late of migration
      (** it holds a version later than this pending one: the file was merged
          after a later one ran *) ]
    (** the database's record disagrees with a source's files, so nothing of
        that source is applied *)
  | `Migration_failed of migration * Rowtype.error
    (** it failed, and the database is at the version before it -- or, for one
        run outside a transaction, at whatever the failure left, and for one
        that ended its own transaction, at whatever that end kept, with the
        migration not recorded *)
  | `No_database of string
    (** the server has no database of this name, or there is no SQLite file of
        this name: it is made with [rowtype-migrate create] *)
  | `Dump of string
    (** [pg_dump] could not run, or failed: in words naming the program, and
        what it wrote as errors, never its arguments, which hold the URL, and
        with the URL and its password masked wherever it said them back *)
  | `Unproved  (** a squash's baseline does not make what the history makes *)
  | `Lock of
    [ `Lost
      (** the connection was lost between two migrations and made again, holding
          the advisory lock no more, so the run stopped: what was applied before
          stays applied, and nothing more was *) ]
    (** what became of the advisory lock Postgres's [apply] migrates under *)
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

(** {1 Sources} *)

val app : string
(** [app], the name the application's own history is recorded under, in every
    database and set by nothing: a name one machine gave and another did not
    would be a history applied again from its start. *)

type source
(** A library's migrations, under the library's name. *)

val source : string -> migration list -> (source, [> error ]) result
(** [source name migrations], for a name of lower-case letters, digits, hyphens
    and underscores starting with a letter, as a package's is. [app] is the
    application's and refused, as is any other name outside that grammar
    ([`Invalid_argument]). *)

val of_package : prefix:string -> string -> (migration list, [> error ]) result
(** [of_package ~prefix package] reads [<prefix>/share/<package>/migrations],
    where a package installs its migrations, as {!of_directory} reads a folder.
    [prefix] is the switch's: the command takes it from [OPAM_SWITCH_PREFIX], or
    else from where it is installed itself. *)

(** {1 The record}

    What every dialect's [apply] and [status] read and write. *)

val default_table : string
(** [schema_migrations], the table the database's record is kept in unless told
    another. Every function that reads or writes the record takes [?table]: a
    name of lower-case letters, digits and underscores, starting with a letter
    or an underscore, after a schema -- on SQLite, an attached database -- and a
    dot if it names one, found as the statements that write it find it, so two
    applications can keep two records in one database. A name outside that
    grammar is refused, since a table's name cannot be a parameter. *)

val table_name : string -> (string, [> error ]) result
(** The name, where it is one {!default_table} allows. *)

type status = {
  source : string;  (** {!app}, or a library's name *)
  applied : (version * string) list;  (** version and name, as recorded *)
  pending : migration list;  (** in the list and not recorded, in order *)
  unknown : version list;
      (** recorded and not in the list: a newer checkout's *)
  edited : version list;  (** recorded, with a text that has changed since *)
  late : version list;  (** pending, and older than one already applied *)
  behind : version option;
      (** the list's baseline, where the database's record stops before it *)
}
(** What a dialect's [status] says of one source. *)

(** {1 A dialect}

    What a database does its own way, for a dialect's library, and nothing else:
    the record's statements are rowtype's, [$1] on every dialect, and so is the
    reading of a record against a list. *)

type transaction_failure =
  [ `Not_committed of string
  | `Conflict of string option
  | `Not_serializable of string
  | `Lost of string
  | `Unsent of string ]
(** What a dialect's transaction may add to its work's failures. *)

(** Whether a database holds the record, and which of the columns a record made
    before them may lack it has. *)
type record_columns = Absent | Present of { checksum : bool; source : bool }

module type Dialect = sig
  type conn

  val run :
    conn -> ('p, 'r) Rowtype.statement -> 'p -> ('r, [> Rowtype.error ]) result

  val exec_raw : conn -> string -> (unit, [> Rowtype.error ]) result

  val within :
    conn ->
    (conn -> ('a, ([> transaction_failure ] as 'e)) result) ->
    ('a, 'e) result
  (** One transaction, which takes the write lock as it begins where the dialect
      lets it choose. *)

  val exclusively :
    lock:int ->
    conn ->
    (holding:(conn -> (unit, [ `Lock of [ `Lost ] | Rowtype.error ]) result) ->
    (unit, error) result) ->
    (unit, error) result
  (** The run, under whatever lock the dialect takes over all of it, and
      [holding], asked at the start of each migration's transaction, which says
      whether the run still holds it. *)

  val prepare_record : table:string -> conn -> (unit, [> Rowtype.error ]) result
  (** The record's table made, and one made before brought up to it. *)

  val record_columns :
    table:string -> conn -> (record_columns, [> Rowtype.error ]) result
  (** Read, and nothing written. *)

  type mark

  val mark : conn -> (mark, [> Rowtype.error ]) result
  (** Inside a migration's transaction, before its file runs. *)

  val unbroken : conn -> mark -> (bool, [> Rowtype.error ]) result
  (** After the file ran: whether the transaction {!val-mark} was taken in is
      still the one open, which a file that ended it -- a [commit], a [rollback]
      -- makes false. *)

  val reset : conn -> (unit, [> Rowtype.error ]) result
  (** What a migration may set for its session put back, after each. *)
end

(** The record's rules over a dialect: what a dialect's library calls its
    [apply] and [status]. *)
module Make (D : Dialect) : sig
  val apply :
    lock:int ->
    table:string ->
    libraries:source list ->
    D.conn ->
    migration list ->
    (unit, [> error ]) result
  (** Every migration the database has not recorded, in version order, each in a
      transaction of its own that first asks the record whether another process
      applied it meanwhile, under {!Dialect.exclusively}; refused as [`Record]
      where the record and the list disagree. *)

  val status :
    table:string ->
    libraries:source list ->
    D.conn ->
    migration list ->
    (status list, [> error ]) result
  (** What {!apply} would do, without doing it, one status for each source in
      the order {!apply} takes them, the application's last; it writes nothing,
      not even the record's table. *)
end

val made_more_than_tables : [> error ]
(** A squash's refusal of a library that made anything but tables, which a
    baseline could not tell from the application's. *)

val squash :
  through:version ->
  migrations:migration list ->
  dumped:(apart:bool -> migration list -> (string, ([> error ] as 'e)) result) ->
  (migration, 'e) result
(** Every migration through the version [through] names, as one baseline:
    [dumped] is a dialect's database of its own, made from nothing, migrated
    through the list it is given and written out -- the schema and the rows,
    never the record -- with the libraries' tables left out where [apart]. The
    baseline is proved before it is answered: it and the migrations after it
    write out the same as the whole history, or it is [`Unproved].
    [`Invalid_argument]: a [through] that is no migration's version, and one
    that is the baseline's with nothing before it. *)

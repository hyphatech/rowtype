(** Migrations on SQLite, by {!Rowtype_migrate}'s files and rules: the library
    [rowtype-migrate.sqlite], built where [rowtype-sqlite] is installed.

    SQLite has no lock over a run: each migration's transaction takes the write
    lock as it begins and first asks the record whether another process applied
    it while this one waited, so two processes migrating at once take turns. A
    savepoint taken before a file runs says whether the file ended its
    transaction. Foreign keys are enforced again after each migration, which one
    outside a transaction may have turned off to rebuild a table. *)

val run :
  < mono_clock : _ Eio.Time.Mono.t ; .. > ->
  string ->
  (Rowtype_sqlite.conn -> ('a, ([> Rowtype_migrate.error ] as 'e)) result) ->
  ('a, 'e) result
(** [run env file f]: a connection to the database in [file], on a switch of its
    own and with the monotonic clock taken from [env], handed to [f] and closed
    when [f] returns or raises. A file that is not there, which SQLite would
    otherwise make, is [`No_database]; [":memory:"] is always there. *)

val create :
  sw:Eio.Switch.t ->
  mono_clock:_ Eio.Time.Mono.t ->
  string ->
  (unit, [> Rowtype_migrate.error ]) result
(** [create ~sw ~mono_clock file]: make the database, an empty file; one that
    exists is refused. *)

val drop : string -> (unit, [> Rowtype_migrate.error ]) result
(** [drop file]: remove the database, if it is there, with the files SQLite
    keeps beside it. *)

val apply :
  ?table:string ->
  ?libraries:Rowtype_migrate.source list ->
  Rowtype_sqlite.conn ->
  Rowtype_migrate.migration list ->
  (unit, [> Rowtype_migrate.error ]) result
(** Apply every migration in the list that the database has not recorded, as
    {!Rowtype_migrate.Make}'s [apply] does. *)

val status :
  ?table:string ->
  ?libraries:Rowtype_migrate.source list ->
  Rowtype_sqlite.conn ->
  Rowtype_migrate.migration list ->
  (Rowtype_migrate.status list, [> Rowtype_migrate.error ]) result
(** What {!apply} would do, without doing it. *)

val dump :
  sw:Eio.Switch.t ->
  mono_clock:_ Eio.Time.Mono.t ->
  ?table:string ->
  ?libraries:Rowtype_migrate.source list ->
  Rowtype_migrate.migration list ->
  (string, [> Rowtype_migrate.error ]) result
(** What the migrations add up to from nothing, the libraries' with the
    application's: a database in memory of its own, migrated, and written from
    its schema table -- each object's statement as SQLite keeps it, brought up
    to date by every [alter table], the tables first and then the views, the
    indexes and the triggers, each kind by name. It needs no file and no server.
*)

val squash :
  sw:Eio.Switch.t ->
  mono_clock:_ Eio.Time.Mono.t ->
  ?table:string ->
  ?libraries:Rowtype_migrate.source list ->
  through:Rowtype_migrate.version ->
  Rowtype_migrate.migration list ->
  (Rowtype_migrate.migration, [> Rowtype_migrate.error ]) result
(** Every migration through the version [through] names, as one baseline, as
    {!Rowtype_migrate.squash} proves one: the schema {!dump} writes, with each
    table's rows as an [insert] of SQLite's own literals ([quote]) after the
    tables and before anything a row would set off, and its foreign keys
    deferred to its commit -- never the record. With [libraries], the tables
    they made are left out; a library that made a view, or a trigger on what it
    did not make, is [`Invalid_argument]. *)

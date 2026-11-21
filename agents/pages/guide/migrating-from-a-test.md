# Migrating from a test

A test migrates its database through the library, which the
[command](migrations.md) is built on. `run` takes Eio's
environment and a target, and closes what it opened; `create`, `drop`,
`dump` and `squash` take the capabilities they use -- a switch, the network
and the clock, and for a dump the process manager -- so a test hands them
nothing more:

```ocaml title="migrate_postgres.ml"
let migrate env target =
  let ( let* ) = Result.bind in
  let* migrations = Rowtype_migrate.of_directory "migrations" in
  Rowtype_migrate_postgres.run env target @@ fun db ->
  Rowtype_migrate_postgres.apply db migrations
```

On SQLite it is `Rowtype_migrate_sqlite`, with a file where Postgres has a
target:

```ocaml title="migrate_sqlite.ml"
let migrate env file =
  let ( let* ) = Result.bind in
  let* migrations = Rowtype_migrate.of_directory "migrations" in
  Rowtype_migrate_sqlite.run env file @@ fun db ->
  Rowtype_migrate_sqlite.apply db migrations
```

`Rowtype_migrate.of_directory` reads every `.sql` file in a directory, in
version order, and refuses a file it cannot read as a migration, naming it.
`apply` applies every migration the database has not recorded, as the
command's `up` does, and `status` says what it would do, without doing it.

## A database of the test's own

`create` makes the database a target names, and `drop` drops it, refused
while anybody is connected to it:

```ocaml title="test_database.ml"
module Pg = Rowtype_postgres

let with_test_database env target test =
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env and mono_clock = Eio.Stdenv.mono_clock env in
  let target = Pg.Conninfo.with_database target "shop_test" in
  let ( let* ) = Result.bind in
  let* () = Rowtype_migrate_postgres.create ~sw ~net ~mono_clock target in
  let result =
    let* () = Migrate_postgres.migrate env target in
    test target
  in
  let* () = Rowtype_migrate_postgres.drop ~sw ~net ~mono_clock target in
  result
```

`Pg.Conninfo.with_database` names another database on the same server. On
SQLite, `Rowtype_migrate_sqlite.create` makes an empty file, and `drop`
removes it with the files SQLite keeps beside it.

## An application never migrates itself

A migration is applied by the command, never by an application at start: a
dialect's `apply` is for a test and a tool. Every instance of an
application would race to migrate, its role would have to own the schema,
and no migration could run ahead of the code that needs it.

A database the server does not have, or a SQLite file that is not there, is
`` `No_database ``, and `Rowtype_migrate.error_to_string` words every
failure for a person, naming the migration and what to do next. Once
migrated, the database is the one to [check statements
against](checking-statements.md).

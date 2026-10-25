---
title: Migrations
---

Plain SQL files named `<UTC stamp>_<name>.sql`, forward only, applied by a
command of their own before your application starts:

```sh
rowtype-migrate create             # make the database
rowtype-migrate new add_users      # migrations/<UTC stamp>_add_users.sql
rowtype-migrate up                 # apply what is pending
rowtype-migrate status [--check]   # applied and pending
rowtype-migrate dump [--check]     # the schema they make, as db/schema.sql
rowtype-migrate squash VERSION     # every migration through it, as one baseline
rowtype-migrate drop               # drop the database, if nobody is connected
```

The command comes with `rowtype-migrate`, and migrates the dialects whose
backend is installed beside it. A command built without a dialect says what
to install for its URL. Every command has `--help`.

## The database

The database is `--url`, or `DATABASE_URL`: a Postgres URL or keyword list,
or `sqlite:PATH`, a file SQLite keeps. `--env` names another variable, and
`--env-file` reads it from a file of `NAME=value` lines, as a `.env` holds
them, rather than from the environment.

The files are in `--dir`, `migrations` unless given. The record of what ran
is the table `--table`, `schema_migrations` unless given, so two
applications can keep two records in one database.

## The files

A migration's version is the UTC stamp in its name, fourteen digits,
`YYYYMMDDHHMMSS`: a timestamp rather than a counter, because two branches
that each add "migration 7" collide on merge where two timestamps do not.
`rowtype-migrate new` writes an empty one stamped with the time now, its
name lower-case letters, digits and underscores. Two files at one version
are refused, since two branches that each added a migration in the same
second are a merge somebody has to look at.

## Applying them

`up` applies each file in its own transaction and records a digest of it,
so two processes migrating at once take turns: Postgres's run holds an
advisory lock, `--lock N` naming another, and each of SQLite's transactions
takes the write lock as it begins and asks the record first. A flag only
Postgres reads -- `--lock`, `--pg-dump`, `--restrict-key` -- is refused for
a `sqlite:` URL, never ignored.

A migration edited after it ran, or merged out of order, is refused, naming
it, and so is a database migrated from a newer checkout; nothing of its
source is applied. A file that ends its transaction itself with a `commit`
or a `rollback` is refused too, since what ran after that end ran outside
the transaction. A migration that fails leaves the database at the version
before it. The way back from a bad migration is the next one.

Your application never migrates itself: the files are applied as a step of
their own before it starts, and `status --check` exits 1 while any migration
is pending, so a script can wait on it. Every instance of an application
migrating at start would race to apply the same files.

## Outside a transaction

A statement a database refuses inside a transaction -- Postgres's
`create index concurrently` or `vacuum`, SQLite's `vacuum` or the
`pragma foreign_keys = off` its table rebuild needs -- goes in a file of its
own whose first line is

```sql
-- rowtype-migrate: no transaction
```

Such a file is one statement, and is recorded after it succeeds, so a run
cut off between the two runs it again: write it to run twice
(`create index concurrently if not exists`).

## The schema, and a squash

`dump` writes what the migrations add up to, migrated from nothing on a
scratch database, never what some database has drifted to: on Postgres, one
made on the server `--url` names and dropped after, written by `pg_dump`,
`--pg-dump` naming another command; on SQLite, one in memory. It is written
to `db/schema.sql`, `-o` naming another file, and changes only when the
schema does. `dump --check` writes nothing, and fails if the file is not
what the migrations make.

A history grows, and a database made from nothing replays all of it.
`squash VERSION` replaces every file through that version with one
baseline, and proves the baseline makes the same schema as the history
before it changes a file. A database that recorded that version has the
baseline already, and what it recorded below it is history: accepted, and
compared with nothing. One whose record stops before it is refused, and is
migrated with a build from before the squash first.

A SQLite dump and squash need no server and no `pg_dump`: each is migrated
in memory and written from SQLite's own schema.

A test migrates its database through the library the command is built on:
see [migrating from a test](/rowtype/guide/migrating-from-a-test/).

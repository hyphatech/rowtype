# AGENTS.md

rowtype is an OCaml library of Hypha Stack. It describes a query's
parameters and rows once, runs it through a backend -- Postgres or SQLite --
and migrates a database of either dialect with the same files and command. This file is for anyone changing it, human or agent. Users start
at [README.md](README.md).

## Commands

```sh
make setup   # once: the switch hypha-rowtype, all dependencies
make test    # starts the test Postgres in Docker, runs every test
make lint    # formatting, odoc, and the release build
make fmt     # format in place
```

`make test` needs `docker compose`. Without
`ROWTYPE_TEST_PG` the suites that need a server are skipped, and the test
output says so; `test_sqlite` needs none, and always runs. CI runs `make lint` and `make test` on OCaml 5.5.

## Layout

```
src/                 rowtype: shapes, statements, the backend signature; no IO,
                     and no transaction, which each backend runs its own way
  shape              the GADT a query's shape is
  rowtype            statements, and the walk of a shape against a backend
postgres/            rowtype-postgres: the backend over postgres-eio, the one
                     place the two meet; pool, listener, transactions, COPY,
                     verify
sqlite/              rowtype-sqlite: the backend over the sqlite3 binding,
                     the one library that links C; transactions
  forms              the text an instant, a date, an interval and an array
                     are kept in, no IO
migrate/             rowtype-migrate: the files, the record and its rules, over
                     one signature a dialect fills
  postgres/          rowtype-migrate.postgres: Postgres's lock, record, reset
                     and pg_dump; built where rowtype-postgres is
  sqlite/            rowtype-migrate.sqlite: SQLite's; built where
                     rowtype-sqlite is
  bin/               the rowtype-migrate command, each dialect it was built
                     with through a dune select
test/
  test_shape         the walk, against a backend that echoes: random shapes
                     and values must come back as themselves; no database
  test_rowtype       statements, transactions and folds over Postgres
  test_types         every scalar at its edges, read in binary and in text,
                     and arrays of any text
  test_errors        what each failure is told as, and no value in any
  test_pool          the pool and the listener
  test_verify        every scalar against every column type
  test_copy          COPY: every value back as itself, a load held a row at
                     a time, and each failure writing nothing
  test_sqlite        the SQLite backend: every scalar against a STRICT column,
                     any text read or refused, placeholders, locks, WAL,
                     cancellation, transactions, the pool on several domains,
                     verify's every branch; no server
  test_migrate       migrations against a real Postgres, dump and squash
                     included
  test_migrate_sqlite the same behaviours on SQLite, dump and squash
                     included; no server
  test_migrate_files the files' pieces and the squash's own rules, which
                     need no database and link no dialect
  command.t          the command, where it needs no database or dialect;
                     command_postgres.t and command_sqlite.t each
                     dialect's, where that dialect is built
  db_target/         where a suite's database comes from: a library, so it
                     reads no variable, prints nothing and never exits
```

Each module's contract is its `.mli`. Read the `.mli` before changing a
module.

## Rules that must hold

Each rule comes with why it exists and the test that catches a break.

- **The `rowtype` package reads no SQL, and names no database.** It links no
  library but `ptime` and `uuidm`, whose types an instant and a uuid are; a
  backend is a package of its own, `rowtype-postgres` or `rowtype-sqlite`,
  and a statement reaches the database as written. Why: a rule of one database's
  syntax in the library every database shares is a rule the next backend
  has to undo. Symptom: `rg -n libraries src/dune` names a library but
  `ptime` and `uuidm`, or `opam install rowtype` installs anything else.
- **`rowtype-sqlite` alone binds C**, through `sqlite3`, and links the
  system's SQLite; every other library is OCaml, and
  `rowtype-migrate.sqlite` reaches SQLite through it alone. Why: SQLite is a library,
  not a server, and has no protocol a driver could speak in OCaml as
  postgres-eio speaks Postgres's, so OCAML.md's pure OCaml over a C binding
  gives way here and nowhere else; `sqlite3` is the binding opam's other
  SQLite packages build on. Symptom: `rg -n -w 'sqlite3|ctypes' -g
  dune src postgres migrate` finds a library, or a `foreign_stubs` anywhere.
- **SQLite is called on a system thread, never on the domain**, and its rows
  are read 64 a hop. Why: a call waits on the disk and on another
  connection's lock, as long as the connection's `wait`, and every fiber of
  the domain would wait with it; a hop costs fifty steps, and 64 rows a hop
  read at half the speed of reading in place, holding no more than 64 rows.
  Test: `a wait leaves the domain` and `ending a statement leaves the
  domain`, in `test_sqlite`, whose fiber that lets the lock go runs only
  while the write waits off the domain.
- **Migrating is one algorithm, and a dialect supplies only its own
  ways.** `Rowtype_migrate`'s record, its refusals and its order are written
  once, over the `Dialect` signature; a dialect gives its lock, its record's
  table, how a migration that ended its own transaction is found, what is
  reset after each, and its dump. The command's `status` refuses by
  `Rowtype_migrate.refusal`, as `up` does, and words nothing of its own. Why:
  two copies of the rules would drift, and a database migrated by one
  dialect's rules and checked by another's is a record nobody can trust.
  Test: `test_migrate` and `test_migrate_sqlite` hold the same behaviours,
  each on its database, and the core's own rules are tested in
  `test_migrate_files`, which links no dialect. Symptom: `rg -n
  'status.behind' migrate/bin` finds a match, or a dialect binds an
  argument of `Dialect`'s as `_`, a value another dialect's rule handed it.
- **A dialect is optional to `rowtype-migrate`.** Its core links
  `rowtype` and no backend; each dialect is an `(optional)` library beside
  it, and the command takes each through a dune `select`, so installing the
  package with one backend links that one alone. Why: a program migrating
  Postgres has no use for SQLite's C library, and DuckDB's will be larger.
  Symptom: a backend among `rowtype-migrate.opam`'s `depends`, or `rg -n
  'rowtype-(postgres|sqlite)' migrate/dune` finding one.
- **A migration is applied by the command, and never by an application.**
  The command `rowtype-migrate up` applies the files from a directory and
  links no application; a dialect library's `apply` is for a test and a tool. Why: every
  instance of an application would race to migrate, its role would have to
  own the schema, and no migration could run ahead of the code that needs
  it. Symptom: an application that migrates itself at start, every instance
  racing to apply the same files.
- **A migration is known by its source and its text.** Each applied one is
  recorded with the history it belongs to -- `app`, or a library's -- and a
  digest of what ran, and a database whose record disagrees with a source's
  files (edited, unknown to `up`, or older than one that source applied) is
  refused by `up`, naming it, before that source is applied. Every check is
  made within one source, never across two. A baseline replaces the files up
  to its version, and what the database recorded below it is history:
  accepted, and compared with nothing. Whether the baseline ran at its
  version is told by the record below it -- a database the baseline made
  has nothing there -- never by the name, since a migration may be named
  anything, `baseline` among them. Why: two databases that ran "the same"
  migration must hold the same schema. Test: `test_migrate` and
  `test_migrate_sqlite`, `a baseline stands for what it replaced` in each.
- **No value is logged, handed to an observer or put in a failure's
  words.** A statement's `debug` line and `?observe` carry its text, with
  placeholders, and never its parameters; a cell that does not decode is
  named by its type, or in SQLite by its kind of value; a data exception,
  class 22, is worded from its code, since Postgres's message quotes the
  value it refused, and so is a SQLite statement's failure as it runs, but
  for a constraint SQLite checks itself, which names columns alone. Why: a
  code, a digest or a token is a parameter, and a failure reaches a log;
  SQLite's functions quote their arguments, each its own way, so no filter
  of its words is ever complete. Test: `no value in a failure or a log`, in
  `test_errors`, and `no value logged`, `no value in a failure` and
  `another kind is refused`, in `test_sqlite`.
- **A transaction's own failures are values.** One that could not begin or
  commit, or whose work answered `Ok` after a failed statement aborted it,
  is `` `Not_committed ``, never a success -- or `` `Conflict ``, named,
  where a deferred constraint refused its `COMMIT`. SQLite's `COMMIT`, when
  it cannot take its lock or a deferred foreign key refuses it, leaves the
  transaction open, so the backend rolls it back and says which. Its
  `COMMIT` and its rollback run to their end whatever cancels the fiber.
  Why: Postgres rolls such a transaction back in silence, and SQLite keeps
  one open that nobody would end; a rollback a cancellation stops before it
  starts leaves the transaction, and its lock, on a connection its caller
  believes is free. Test: `transactions` in `test_rowtype` and in
  `test_sqlite`, `cancelled while waiting` among them.
- **The one module `open` is `open Shape` in `src/rowtype.ml`, and a local
  open is a span's alone**, `Mtime.Span.(10 * s)`. Why: the walk exists to
  pattern-match that GADT, and every constructor qualified would bury the
  match; a span's operators are made to be opened around the one
  expression, as OCAML.md writes one. Symptom:
  `rg -n '^\s*open |\b[A-Z]\w*\.[(\[{]' -g '*.ml' src postgres sqlite migrate | rg -v 'Mtime\.Span\.\('`
  finds anything but `open Shape`.

## Changes and releases

Its four packages are released together. A migration file's directive
and the record's table are behaviour: changing either is breaking.

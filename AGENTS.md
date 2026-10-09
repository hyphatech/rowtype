# AGENTS.md

rowtype describes a query's parameters and rows once, runs it through a
backend, and migrates a Postgres database. This file is for anyone changing
it, human or agent. Users start at [README.md](README.md).

## Commands

```sh
make setup   # once: local opam switch in ./_opam, all dependencies
make test    # starts the test Postgres in Docker, runs every test
make lint    # formatting, odoc, and the release build
make fmt     # format in place
```

`make test` needs `docker compose`. Without
`ROWTYPE_TEST_PG` the suites that need a server are skipped, and the test
output says so. CI runs `make lint` and `make test` on OCaml 5.5.

## Layout

```
src/                 rowtype: shapes, statements, the backend signature; no IO
  shape              the GADT a query's shape is
  rowtype            statements, and the walk of a shape against a backend
postgres/            rowtype-postgres: the backend over postgres-eio, the one
                     place the two meet; pool, listener, transactions, COPY,
                     verify
migrate/             rowtype-migrate: the files, the record, up, dump, squash
  bin/               the rowtype-migrate command
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
  test_migrate       migrations against a real Postgres, dump and squash
                     included
  test_migrate_files the files' pieces, which need no database
  command.t          the command, where it needs no database
  test_style         the house rules that can be checked mechanically
  db_target          where a suite's database comes from
```

Each module's contract is its `.mli`. Read the `.mli` before changing a
module.

## Rules that must hold

Each rule comes with why it exists and the test that catches a break.

- **`rowtype` reads no SQL, and names no database.** It links no library
  but `ptime` and `uuidm`, whose types an instant and a uuid are; a backend is a package of its own, `rowtype-postgres`, and a
  statement reaches the database as written. Why: a rule of one database's
  syntax in the library every database shares is a rule the next backend
  has to undo. Test: `rowtype links ptime and uuidm alone`, in
  `test_style`, reads `src/dune`'s libraries, and `opam install rowtype`
  installs nothing else.
- **A migration is applied by the command, and never by an application.**
  `rowtype-migrate up` applies the files from a directory and links no
  application; `Rowtype_migrate.run` is for a test and a tool. Why: every
  instance of an application would race to migrate, its role would have to
  own the schema, and no migration could run ahead of the code that needs
  it.
- **A migration is known by its text.** Each applied one is recorded with a
  digest of what ran, and a database whose record disagrees with the files
  (edited, unknown to `up`, or older than one applied) is refused by `up`,
  naming it, before anything is applied. A baseline replaces the files up
  to its version, and what the database recorded below it is history:
  accepted, and compared with nothing. Why: two databases that ran "the
  same" migration must hold the same schema. Tests: `test_migrate`.
- **No value is logged, handed to an observer or put in a failure's
  words.** A statement's `debug` line and `?observe` carry its text, with
  placeholders, and never its parameters; a cell that does not decode is
  named by its type; a data exception, class 22, is worded from its code,
  since Postgres's message quotes the value it refused. Why: a code, a
  digest or a token is a parameter, and a failure reaches a log. Test: `no
  value in a failure or a log`, in `test_errors`.
- **A transaction's own failures are values.** One that could not begin or
  commit, or whose work answered `Ok` after a failed statement aborted it,
  is `` `Not_committed ``, never a success. Why: Postgres rolls such a
  transaction back in silence. Tests: `transactions` in `test_rowtype`.
- **The one `open` is `open Shape` in `src/rowtype.ml`.** Why: its walk
  exists to pattern-match that GADT, and every constructor qualified would
  bury the match. Test: `test_style` names it and refuses any other.

## House style

The house style, the OCaml checklist and the OCaml tools are
[OCAML.md](OCAML.md), the same in every Hypha OCaml repository and changed
in all of them together, never in one. It is imported here, so it is read
with this file:

@OCAML.md

## Git

How work becomes a commit -- its format, where it happens, what is pushed
-- is [GIT.md](GIT.md), the same in every Hypha repository and changed in
all of them together, never in one. It is imported here, so it is read
with this file:

@GIT.md

## Changes

- A user-visible change adds a line under `## Unreleased` in
  [CHANGES.md](CHANGES.md), in the same commit. A breaking one says so.
- A change that makes a sentence in a document false edits that sentence in
  the same commit.
- A user-visible change updates the `.mli` it touches; a new supported
  feature or a removed limitation updates the README.

## Releases

[Semantic Versioning 2.0.0](https://semver.org). Before 1.0, a breaking
change bumps the minor version and anything else the patch. The three
packages are released together, at one version.

Breaking means a user's code may stop compiling or behave differently:
removing or renaming anything in an `.mli`, changing a type, adding a
constructor to a public variant (it breaks exhaustive matches), adding a
required argument, or changing a default or documented behaviour. Adding a
function, a module or an optional argument is not breaking, and neither is
the wording of an error or a log line. A migration file's directive and the
record's table are behaviour: changing either is breaking.

The version lives only in the git tag (`0.1.0`, no `v`). A release renames
`## Unreleased` in CHANGES.md to the version and date, and tags it. The GitHub
release notes are that entry with each paragraph and bullet on one line,
since GitHub keeps every line break in release notes.

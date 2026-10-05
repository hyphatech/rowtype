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

Nothing is on PATH: run OCaml tools through the Makefile, or prefix them
with `opam exec --switch=. --`. `make test` needs `docker compose`. Without
`ROWTYPE_TEST_PG` the suites that need a server are skipped, and the test
output says so. CI runs `make lint` and `make test` on OCaml 5.4 and 5.5.

**Finding a name's uses.** `make setup` installs Merlin. Build the index
once, `dune build @ocaml-index` through the switch, and then
`ocamlmerlin single occurrences -identifier-at LINE:COL -scope project
-filename FILE < FILE` lists every use of the name at that point, tests
included; `outline` lists a module's values and types without reading it,
and `type-enclosing -position LINE:COL` gives the type there. Each answers
in JSON, where `rg` would need every hit read for a shadowed name.

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
  has to undo. Test: `src/dune` names `ptime` and `uuidm` alone, and `opam install
  rowtype` installs nothing else.
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

## House style

The rules are ranked, because they conflict:

1. **Simple and obvious beats clever.** If a reviewer has to reconstruct
   why something works, it is wrong even when it is correct.
2. **Locality of behaviour beats DRY.** Code that changes together lives
   together, and a function reads top to bottom. Code that only looks alike
   is not duplication.
3. **No layer without a job.** No abstraction with one implementation
   unless the signature is the point (`Backend` is), no functor for a
   choice made once.
4. **Comments say why, never what**, in a sentence or two: the protocol
   section, a constraint that is not visible, the reason for a number.
   Never history; that is the commits'. A comment the names already say is
   deleted.

### OCaml checklist

A change is done when every box holds:

- [ ] `make lint` and `make test` pass.
- [ ] **A change brings its tests**: the typical corner cases (empty, one,
  the boundaries, invalid input, a failure partway through), a property
  test wherever a round trip exists, and the real server wherever a test
  can run one, never a mock of it; a stub stands in only for a third
  party's service.
- [ ] **No partial functions**: nothing raises on an input the code has not
  ruled out. No `failwith`, `invalid_arg`, `Option.get`, `Result.get_ok`,
  `List.hd`, `List.tl`, `List.nth`, `Obj.magic`; and a stdlib call that
  raises -- `String.sub`, an index, `Hashtbl.find`, `List.assoc`,
  `int_of_string`, `Char.chr`, `List.combine` -- only on an input already
  known to be in range, else its `_opt`. Errors are values: a `result` with
  a variant error, and `let*` over it.
- [ ] **No polymorphic `compare`**, and no `=` on a type that has a module:
  `Int.compare`, `String.equal`, `Char.equal`. `=` on `int` is fine. It also
  hides in `List.mem`, `List.assoc`, `List.sort compare`, `max`, `min` and a
  `Hashtbl`'s keys: accepted over plain data -- an `int`, a `char`, a
  `string` -- where nothing can hold a closure or an abstract type, and this
  rule broken over anything else.
- [ ] **No `open`**, local ones (`M.( ... )`) included. Alias modules
  instead: `module S = Rowtype`. The one exception is `open Shape` in
  `src/rowtype.ml`, whose walk exists to pattern-match that GADT;
  `test_style` names it.
- [ ] **No silenced warnings.** The warning set in `dune` is the linter, and
  a warning that looks wrong is a code shape that is wrong.
- [ ] **An `.mli` per library module.** Abstract types, hidden
  constructors; the contract in odoc in the `.mli`, the reasons in the
  `.ml`. It exports what a user needs, and nothing more.
- [ ] **The library never prints, exits or reads the environment.** It logs
  on its own `Logs` source. The command in `migrate/bin/` is where the
  environment is read.
- [ ] **A meaning is a type.** A state is a variant, never a string or a
  boolean; a unit or an identifier that travels unnamed -- a column, an
  element, a returned value -- is a type of its own, never a bare `int` or
  `string` whose meaning the caller has to remember. A labelled argument
  that names its unit at every call (`~timeout_s`) is enough.
- [ ] **Advanced types only where they delete real duplication.** A GADT
  earns its place by describing a thing once that would otherwise be
  described twice; otherwise, records and variants.
- [ ] **Cancellation leaves nothing held.** A fiber cancelled at any effect
  releases what it held: a connection goes back to its pool or is closed,
  and a lock is let go. A catch-all handler (`with _ ->`,
  `| exception _ ->`) re-raises `Eio.Cancel.Cancelled` before anything else,
  or it swallows the cancellation.
- [ ] **Labelled arguments** where a call would otherwise be ambiguous, and
  optional arguments with defaults, followed by `()`.
- [ ] **Stdlib naming**: `t`, `create`/`make`, `of_x`/`to_x`, `*_opt`,
  stdlib argument order.
- [ ] **A name says what a thing is or does.** No metaphors or
  abbreviations beyond the stdlib's (`b` a buffer, `n` a count, `f` a
  function).
- [ ] **A number with a reason is a named constant**, the reason beside it.
- [ ] **No needless cost.** No quadratic walk where a linear one is as
  clear, and no whole result held where streaming is as simple. A claim
  about speed comes with a measurement.
- [ ] **Plain stdlib.** No Base, Core or Lwt.
- [ ] **`ocamlformat` decides layout.** Never format by hand.

## Changes

- A user-visible change adds a line under `## Unreleased` in
  [CHANGES.md](CHANGES.md), in the same commit. A breaking one says so.
- A change that makes a sentence in a document false edits that sentence in
  the same commit.
- A user-visible change updates the `.mli` it touches; a new supported
  feature or a removed limitation updates the README.
- Commit subjects are imperative, under 72 characters, with no full stop.
  The body says why, wrapped at 72. No trailers.

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
`## Unreleased` in CHANGES.md to the version and date, tags it, and submits
the packages to opam-repository from the `hyphatech` fork. The GitHub
release notes are that entry with each paragraph and bullet on one line,
since GitHub keeps every line break in release notes.

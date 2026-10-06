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
output says so. CI runs `make lint` and `make test` on OCaml 5.4 and 5.5.

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
- **The one `open` is `open Shape` in `src/rowtype.ml`.** Why: its walk
  exists to pattern-match that GADT, and every constructor qualified would
  bury the match. Test: `test_style` names it and refuses any other.

<!-- hypha-ocaml: begin. Every Hypha OCaml repository carries this text word for word; a change to it is made to every copy together. -->
## House style

The goal is code that is beautiful from the inside: idiomatic, clean and
simple. An OCaml expert who has never seen the repository recognises every
pattern in it on sight and is surprised by nothing.

The rules are ranked, because they conflict:

1. **Simple and obvious beats clever.** If a reviewer has to reconstruct
   why something works, it is wrong even when it is correct.
2. **Locality of behaviour beats DRY.** Code that changes together lives
   together, and a function reads top to bottom without chasing helpers
   around the file. Code that only looks alike is not duplication when it
   changes for different reasons. Extract only for a rule that must hold
   in exactly one place, a boundary the code cannot cross -- two
   executables that must not link each other -- or a third copy that has
   already drifted.
3. **No layer without a job.** No abstraction with one implementation
   unless the signature is the point, no functor for a choice made once,
   no indirection added for symmetry. A 40-line function doing one thing
   beats four 10-line ones only ever called in sequence.
4. **Comments say why, never what**, in a sentence or two: the RFC or
   protocol section, a rule the code must keep, a constraint that is not
   visible, the measured reason for a number. Never history; that is the
   commits'. A comment that explains what the code does means the code is
   rewritten, and one the names already say is deleted.

### OCaml checklist

A change is done when every box holds:

- [ ] The checks under *Commands* pass.
- [ ] **A change brings its tests**: the typical corner cases (empty, one,
  the boundaries, invalid input, a failure partway through), a property
  test wherever a round trip exists, and the real server wherever a test
  can run one, never a mock of it; a stub stands in only for a third
  party's service.
- [ ] **No partial functions**: nothing raises on an input the code has not
  ruled out. No `failwith`, `Option.get`, `Result.get_ok`, `List.hd`,
  `List.tl`, `List.nth`, `Obj.magic`, and `invalid_arg` only where the
  `.mli` says it raises and the repository's rules name it; a stdlib call
  that raises -- `String.sub`, an index, `Hashtbl.find`, `List.assoc`,
  `int_of_string`, `Char.chr`, `List.combine` -- only on an input already
  known to be in range, else its `_opt`.
- [ ] **Errors are values**: a `result` with a variant error, and `let*`
  over it rather than nested matches. Eio is direct-style, so `let*` always
  means `result`. An exception is a programmer's error and never crosses a
  library boundary.
- [ ] **No polymorphic `compare`**, and no `=` on a type that has a module:
  `Int.compare`, `String.equal`, `Char.equal`. `=` on `int` is fine. It also
  hides in `List.mem`, `List.assoc`, `List.sort compare`, `max`, `min` and a
  `Hashtbl`'s keys: accepted over plain data -- an `int`, a `char`, a
  `string` -- where nothing can hold a closure or an abstract type, and this
  rule broken over anything else.
- [ ] **No `open`**, local ones (`M.( ... )`) included. Alias modules at
  the top of the file instead (`module P = Protocol`), and annotate a
  value's type once rather than qualify its fields (`(g : Store.game)`,
  then `g.size`, never `g.Store.size`). The exceptions are a module made to
  be opened -- binding operators and nothing else, or combinators whose
  `.mli` says they are written inside `M.( ... )` -- and an `open` the
  repository's rules name.
- [ ] **No silenced warnings.** The warning set in `dune` is the linter --
  warning 9 makes adding a record field a compile error at every pattern
  that should handle it -- and a warning that looks wrong is a code shape
  that is wrong.
- [ ] **Ergonomics is a requirement, never a polish**, and a refactoring or
  a new feature that ignores it is not done. It is judged where it is
  used -- the tests, the examples, the README, every caller -- as much as
  in its own module: the common case reads in one obvious line, a caller
  writes nothing the code could have known, a mistake is a compile error
  or a refusal that says what to do, every name, label and argument order
  is the one a caller would guess, and there is one way to do a thing: a
  new name never repeats what the caller can already say with the names it
  has. A change that leaves a caller's code longer, noisier or easier to
  get wrong is redone, however clean its inside.
- [ ] **An `.mli` per library module.** Abstract types, hidden
  constructors; the contract in odoc in the `.mli`, the reasons in the
  `.ml`. It exports what a user needs, and nothing more: an export used
  nowhere outside its module, or only by its tests, is not exported.
- [ ] **A library never prints or reads the environment, and exits only
  where its `.mli` says.** An executable reads its environment where it
  starts. A library logs on its own `Logs` sources.
- [ ] **A meaning is a type.** A state is a variant, never a string, a
  boolean or a pair of booleans one combination of which is impossible; a
  unit or an identifier that travels unnamed -- a column, an element, a
  returned value -- is a type of its own, never a bare `int` or `string`
  whose meaning the caller has to remember. A labelled argument that names
  its unit at every call (`~timeout_s`) is enough.
- [ ] **Advanced types only where they delete real duplication.** A GADT
  earns its place by describing a thing once that would otherwise be
  described twice; otherwise, records and variants.
- [ ] **Effects at the edge.** What can be computed without IO is, in code
  that does none, and a value is converted to and from a wire format at a
  boundary, never in the middle.
- [ ] **Cancellation leaves nothing held.** A fiber cancelled at any effect
  releases what it held: a connection goes back to its pool or is closed,
  and a lock is let go. A catch-all handler (`with _ ->`,
  `| exception _ ->`) re-raises `Eio.Cancel.Cancelled` before anything else,
  or it swallows the cancellation.
- [ ] **Labelled arguments** where a call would otherwise be ambiguous, and
  optional arguments with defaults, followed by `()`.
- [ ] **Stdlib naming**: `t`, `create`/`make`, `of_x`/`to_x`, `*_opt`,
  stdlib argument order.
- [ ] **A name says what a thing is or does, in the words a person would
  use where it is read.** No metaphors, moods or puns, and no
  abbreviations beyond the stdlib's (`b` a buffer, `n` a count, `f` a
  function). A rename earns itself at a use site: it is made only where a
  caller's reader misreads the current name or has to look it up, never
  because a rule can be cited for it, and never to tell apart two names
  the types already keep apart. A name assembled from parts to satisfy a
  rule (`renewals_per_idle`, `Call_failed`) is worse than none: where no
  natural name comes, the plainer one stays -- the one already there, or
  none.
- [ ] **A number with a reason is named where nothing beside it already
  says it**, the reason beside it: a field, a label or a comment that
  names its unit and purpose (`send_timeout_s = 10.`) needs nothing more.
- [ ] **No needless cost.** No quadratic walk where a linear one is as
  clear, and no whole result held where streaming is as simple. A claim
  about speed comes with a measurement.
- [ ] **Plain stdlib.** No Base, Core or Lwt.
- [ ] **`ocamlformat` decides layout.** Never format by hand; when its
  output is ugly, the code's shape is what is wrong.
- [ ] **What a change touches is found by the compiler's knowledge**, not
  by a text search: every caller of a changed signature and every user of
  an export is the language server's references, or Merlin's
  `occurrences`, below.

## OCaml tools

Nothing is on PATH. Every OCaml tool runs through the repository's local
switch, `opam exec --switch=<root> --` from the repository's root, or
through the Makefile; no `eval`. `make setup` installs Merlin and
`ocaml-lsp-server` with the rest.

**The compiler's knowledge reaches an agent through `ocamllsp`**, where
`rg` matches text and a shadowed, re-exported or aliased name defeats it.
Run as the agent's language server, from the repository's own switch,
it answers every edit to OCaml source with its type errors, and its `LSP`
tool gives a name's definition, its references, tests included, its type
and a module's symbols. References read the index as the last build left
it, so after an edit rebuild it -- `dune build @check @ocaml-index` --
before asking.

**A worktree inside the checkout is its own dune root only with an
untracked `dune-workspace`**, holding the `dune-project`'s own `(lang dune
...)` line and kept out of version control. Without one dune takes the
checkout around it as the root and skips the hidden directory the worktree
is in, so the server and Merlin answer from no configuration: every module
unbound, one use of every name. For a single command, `DUNE_ROOT` set to
the worktree does the same.

**Without the language server, Merlin's command line answers the same**:
`ocamlmerlin single <query> -filename FILE < FILE`, in JSON, lines from 1
and columns from 0 -- `occurrences -identifier-at LINE:COL -scope
project`, `locate -position LINE:COL`, `type-enclosing -position LINE:COL`,
`outline`, and `errors`, which reads the file from standard input. Its
`occurrences` reads the index as the last `dune build @ocaml-index` left
it.

**Ask for a record field's uses from its definition in the `.ml` or from
a use**, never from its declaration in the `.mli`, which answers with that
declaration alone; a value asked from its `.mli` finds every use.

`dune describe` lists every library, executable and module, so nothing is
missed when the whole project is read.

**Ask the language server who uses a name and what it is; ask `rg`
everything else, always with a path** -- with none it reads standard
input, which an agent's shell never closes.

**Search with `rg`, never `grep -r` or `find`.** `_build/` and `_opam/` are
gitignored, so `rg` skips them, where `find . -name '*.ml'` also returns
every copy of the source under `_build/` and every package under `_opam/`.

`opam list --installed` says what the switch holds; `opam list
--required-by --recursive` resolves against what is available, not what is
installed.
<!-- hypha-ocaml: end -->

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

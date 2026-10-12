# Changes

## 0.5.0 (2026-10-12)

- A library's migrations are a history of their own: a library ships them
  in its package, `rowtype-migrate --source <package>` (or
  `--source <name>=<dir>`) applies them before the application's, and each
  source is ordered and checked only among its own, so a library's
  migration is never late beside an application's. `Rowtype_migrate.run`,
  `dump` and `squash` take `?libraries`; `source` and `of_package` make
  one; `squash` leaves a library's tables out of an application's
  baseline.
- Breaking: the record's table gains a `source` column, and its key is the
  source and the version. A record made before is given them on its next
  `up`, every row the application's, `app`; nothing else is asked of it.
- Breaking: `Rowtype_migrate.status` answers a status for each source, the
  application's last, and `status` has a `source` field.
- `rowtype-migrate new` writes one line, asking what the migration changes
  and why; it no longer says in every file that a migration is forward only,
  which the command holds.

## 0.4.0 (2026-10-11)

- Breaking: `commit` leaves `Rowtype.Backend`. A backend runs its own
  transactions, since databases differ in how one begins and what a
  `COMMIT` after a failed statement does, and `Rowtype.Make` runs none.
  A backend that defines `commit` still compiles, the value now its own;
  code that calls `B.commit` through a `Rowtype.Backend`, in a functor
  over one, does not, and calls its backend's transaction instead.
- Breaking: in `rowtype-postgres`, every duration is an `Mtime.Span.t`
  named for what it bounds, not a float of seconds named for its unit, so
  a wait in the wrong unit no longer compiles. `connect`'s and
  `Listener.connect`'s `?timeout_s` is `?timeout`, `Listener.connect`'s
  `?heartbeat_s` is `?heartbeat`, `Pool.create`'s `?timeout_s`,
  `?wait_s`, `?max_lifetime_s` and `?idle_check_s` are `?timeout`,
  `?wait`, `?max_lifetime` and `?idle_check`, and `Pool.use`'s `?wait_s`
  is `?wait`: write `~timeout:Mtime.Span.(10 * s)`.
- Breaking: in `rowtype-postgres`, `timeout_s` is `timeout`, and
  `set_timeout_s conn t` is `set_timeout conn ~timeout:t`, with
  `t : Mtime.Span.t option`, as postgres-eio writes it.
- `rowtype-postgres` asks for postgres-eio 0.3.0, and pins it at that tag.
  Breaking: the `Conninfo.t` field `connect_timeout_s : float option`,
  which `Rowtype_postgres.Conninfo` re-exports, is
  `connect_timeout : Mtime.Span.t option`; a connection string still
  gives it in whole seconds.
- `rowtype-migrate` links `mtime`, for the ten seconds its commands give
  a connect.

## 0.3.0 (2026-10-11)

- In `rowtype-migrate`, a record's table refused for its name says the
  name starts with a letter or an underscore, as its odoc now does too:
  `1migrations`, all of whose characters were allowed, was refused with
  words that named only the characters.
- In `rowtype`, `Rowtype.error`'s `` `Closed `` is `` `Connection `Closed ``,
  the connection's state under the connection, so a program that merges
  it with a pool's error reads which one closed. Its words are unchanged.
  Breaking: a match over the error names `` `Connection `Closed ``.
- In `rowtype-postgres`, `Pool.error` is
  `` [ `Pool of [ `Busy | `Closed ] ] ``, the pool's states under the pool:
  `Pool.use` answers `` `Pool `Busy `` where it answered `` `Busy ``, and
  `` `Pool `Closed `` at once for a closed pool, where it waited out
  `wait_s` and answered `` `Busy ``, and `Pool.error_to_string` is
  postgres-eio's. Breaking: a match on what `Pool.use` answers names
  `` `Pool ``.
- In `rowtype-postgres`, every failure it answers has a printer beside it:
  `Transaction.error_to_string` for what `Transaction.within` answers, its
  own failures and a statement's, `Pool.error_to_string` for a
  `Pool.error`, and `error_to_string` for what `verify` answers,
  `` `Disagreements `` included. A statement's failure is told as `Rowtype.error_to_string`
  tells it.
- Breaking: OCaml 5.5 or later is required. The opam files asked for 5.4,
  which CI tested and nothing else did; every switch the code is written
  in is 5.5.
- The `rowtype-postgres` package pins postgres-eio to its 0.2.0 tag from its own opam
  file, so pinning rowtype is the whole install.
- `rowtype-postgres` asks for postgres-eio 0.2.0, and what changed there
  reaches it. Breaking: `Conninfo.protocol_minor`, which `Rowtype_postgres.Conninfo`
  re-exports, is gone; it was a step of the driver's start-up. Breaking: a
  fiber cancelled while `Pool.use` runs its function stops at once, its
  connection closed and made again at the next lend, where the function
  ran to its end; and a `Listener` whose server is restarting waits for it,
  where `next` answered the server's refusal.
- In `rowtype-postgres` and `rowtype-migrate`, a connection string whose
  password holds an unquoted space is refused without repeating the words
  after the space, which are the password's: `password=ab12 cd34` was
  refused naming `"cd34"`. The refusal says to quote the password.
- In `rowtype-postgres`, a fiber cancelled inside `Transaction.within`
  between statements -- at a sleep, a wait, anything but a statement in
  flight -- closes the connection, which ends the transaction, where it
  left the transaction open, with its locks, and the next `within` on it
  refused. The next `within` makes the connection again.
- In `rowtype-migrate`, a migration file that ends its own transaction -- a
  `commit` or a `rollback` in it -- is refused, and its record removed,
  where it was recorded as applied even when what followed its end failed.
  The refusal names the `-- rowtype-migrate: no transaction` directive.
  Breaking: such a file, which applied before, is now refused. A migration
  runs through `Transaction.within`, once.
- In `rowtype`, a row with more columns than its shape reads is refused, as
  one with fewer is and as `verify` refuses the statement, where the
  columns after the shape's were dropped: `select 1, 2` read as `S.int`
  answered 1. Breaking: such a read, a `find` of `S.unit` over a
  `select` included, is now refused; a statement whose rows are not read
  is an `exec`.
- In `rowtype-postgres`, a value its scalar cannot hold -- an `int8` past 63
  bits read as `S.int`, infinity, a year outside 1 to 9999 -- is refused
  as what it is, naming what reads it, `Rowtype.int64` or a NULL read
  with `Rowtype.opt`, where it was told as a type it is not ("expected
  INT, got a value of type 20").
- Breaking: `commit` leaves `Rowtype.S`, and so `Rowtype_postgres`, since a
  `begin` and a `commit` sent by hand lose what `Transaction.within` tells
  apart: a `COMMIT` that rolled back an aborted transaction. It stays in
  `Rowtype.Backend`. `exec_raw` is for DDL and a session's settings, and a
  transaction is `Transaction.within`'s.

## 0.2.0 (2026-10-06)

- `rowtype-postgres`: an array's element is read as its own type is, so
  a `real[]` holds the same floats a `real` does, and a `text[]` is
  refused as an array of `int`, `bool` or JSON as a `text` is. Breaking:
  a statement that read the wrong type through an array is now refused.
- `rowtype-postgres`: a read accepts exactly the types `verify` does, in
  binary and in text alike, where the driver alone read any type with no
  binary form as its text -- a `numeric` as an `int`, an enum as JSON --
  and `S.text` read a `bool`, an integer, or with no statement cache a
  `date`. An enum is read by `S.enum`, below, and any other type the
  database made through a cast, `::text`, which its refusal names.
  Breaking: such reads are refused, and an enum read as `S.text` needs
  `S.enum` or the cast.
- `rowtype`: `S.enum label values` reads and writes an enum through the
  labels of the program's own type, with no cast in the SQL, alone or in
  an array; a row holding a label none of `values` has is refused, naming
  its column. `rowtype-postgres`: `verify` holds such a column to an enum
  that has every one of the labels, and refuses a list of none or one that
  repeats a label; a label only the database has is no disagreement, so a
  migration may add one ahead of the code. Breaking for a backend:
  `Enum` joins `Rowtype.scalar`.
- `rowtype`: `` `Closed `` and `` `Lost `` join `Rowtype.error`. A
  statement on a closed connection is `` `Closed ``, sent nothing and did
  nothing; one whose connection failed in flight -- a timeout, a broken
  socket, a FATAL from the server -- is `` `Lost ``, and may have taken
  effect. Both were `` `Db ``. Breaking: a match over the error needs both.
- `rowtype-postgres`: a transaction whose `COMMIT` was lost in flight
  answers `` `Lost ``, which joins `Transaction.failure`, where it
  answered `` `Not_committed `` though the server may have committed.
  Breaking.
- `rowtype-postgres`: a fiber cancelled with a statement in flight on a
  connection of its own asks the server to cancel the statement, as a
  pooled connection already did, where the statement ran on to its end
  for nobody, holding its locks and committing what it wrote.
- `rowtype`: `run_many` runs an `exec` statement once for each value, in
  one round trip and all or nothing, and a backend's `batch` is what it
  runs on. Breaking for a backend: `batch` is required.
- `rowtype-postgres`: `Listener.unlisten` stops hearing a channel, and
  `Listener.connect` takes `?parameters`, as the driver's does.
- `rowtype-postgres`: the documentation says that a raise from `fold`'s
  function closes the connection.

## 0.1.0 (2026-10-06)

First release.

- `rowtype`: a query's parameters and rows described once, binding and
  decoding both, with every failure a polymorphic variant; integers to
  the whole of an `int8`, floats, text, bytes, bools, instants, dates and
  timestamps as `Ptime` values to the microsecond, intervals as their
  months, days and microseconds, uuids as `Uuidm.t`, JSON, options,
  arrays and your own types; `fold` over a result without holding it.
- `rowtype-postgres`: the Postgres backend over postgres-eio, with a pool,
  a listener, transactions with serializable retries and their own
  failures named as a type, statements checked against the database, and
  `copy_in`, a load of any size bound by a shape and streamed through one
  COPY.
- `rowtype-migrate`: plain SQL migrations, forward only, applied under an
  advisory lock and known by their digests, every refusal and failure a
  polymorphic variant naming its migration; a schema dump, and squashing
  into a baseline.

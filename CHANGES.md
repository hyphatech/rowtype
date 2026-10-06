# Changes

## 0.2.0 (2026-10-06)

- `rowtype-postgres`: an array's element is read as its own type is, so
  a `real[]` holds the same floats a `real` does, and a `text[]` is
  refused as an array of `int`, `bool` or JSON as a `text` is. Breaking:
  a statement that read the wrong type through an array is now refused.
- `rowtype-postgres`: a read accepts exactly the types `verify` does, in
  binary and in text alike, where the driver alone read any type with no
  binary form as its text -- a `numeric` as an `int`, an enum as JSON --
  and `S.text` read a `bool`, an integer, or with no statement cache a
  `date`. A type the database made, an enum among them, is read through a
  cast, `::text`, which its refusal names. Breaking: such reads are
  refused, and an enum read as `S.text` needs the cast.
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

# Changes

## 0.1.0 (2026-10-05)

First release.

- `rowtype`: a query's parameters and rows described once, binding and
  decoding both, with every failure a polymorphic variant; integers,
  floats, text, bytes, bools, instants to the microsecond, uuids, JSON,
  options, arrays and your own types; `fold` over a result without
  holding it.
- `rowtype-postgres`: the Postgres backend over postgres-eio, with a pool,
  a listener, transactions with serializable retries, statements checked
  against the database, and `copy_in`, a load of any size bound by a shape
  and streamed through one COPY.
- `rowtype-migrate`: plain SQL migrations, forward only, applied under an
  advisory lock and known by their digests; a schema dump, and squashing
  into a baseline.

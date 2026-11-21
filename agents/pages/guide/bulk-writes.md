# Bulk writes

Three ways write many rows, each in one round trip: a list bound as an
array, one statement run for each value, and a load through `COPY`.

## A list bound as an array

A list bound as an array writes many rows in one statement and one round
trip:

```ocaml title="add_all.ml"
module S = Rowtype
module Pg = Rowtype_postgres

let add_users =
  S.exec
    ~params:S.(t2 (array text) (array text))
    "insert into users (name, email) select * from unnest($1::text[], \
     $2::text[])"

let add_all db users = Pg.run db add_users (List.split users)
```

For a few rows, this is the simplest: one statement, which
[`verify`](checking-statements.md) checks as any other.

## One statement for each value

`Pg.run_many db statement values` runs an `exec` statement once for each
value in one round trip, all or nothing: if any run fails, none applies.

```ocaml title="add_each.ml"
module S = Rowtype
module Pg = Rowtype_postgres

let add_user =
  S.exec
    ~params:S.(t2 text text)
    "insert into users (name, email) values ($1, $2)"

let add_each db users = Pg.run_many db add_user users
```

A statement that answers a row is refused before anything is sent, since a
batch reads no rows.

## A load through COPY

`Pg.copy_in` loads rows of any number with one `COPY`, each bound by its
shape as a statement's parameters are, and read from a `Seq.t` as they are
sent, so a load is held a row at a time:

```ocaml title="load.ml"
module S = Rowtype
module Pg = Rowtype_postgres

let load db users =
  Pg.copy_in db ~table:"users" ~columns:[ "name"; "email" ]
    S.(t2 text text)
    (List.to_seq users)
```

It answers how many rows it wrote. `~columns` are in the row's order, every
column in table order where empty, and quoted as given, so `"Jobs"` is not
`jobs`; a number of them that is not the row's is refused before anything
is sent. `?schema` names the table's schema.

A row that cannot be bound, a constraint, and a raise from the sequence each
fail the whole `COPY`, with nothing written and the connection usable; the
raise passes. A cancelled load closes the connection and cancels the `COPY`,
as for any statement.

On SQLite, `Sq.copy_in` loads rows of any number from a `Seq.t` with one
prepared `insert`, all or none, as `Pg.copy_in` does with `COPY`. Inside a
transaction, the load is the transaction's, and goes with it.

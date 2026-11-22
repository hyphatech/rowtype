# Statements

A statement is its SQL, the shape of its parameters and how many rows it
answers, in one value. It is declared once, beside the function that runs
it, and run by a backend's `run`, whose answer is the type the statement
says.

## The SQL

A statement reaches the database as written, in its own syntax. `rowtype`
reads no SQL. A placeholder is `$1`, `$2`, ... bound by position from the
shape's values, and `$1` twice asks about one value twice. Whether a
statement and its values agree in number is the database's to check, and
[`verify`'s](checking-statements.md) before any runs.

## How many rows it answers

| Statement | Answers |
|---|---|
| `S.exec ~params sql` | `unit` |
| `S.find ~params ~row sql` | exactly one row; none or several is an error |
| `S.find_opt ~params ~row sql` | at most one row; several is an error |
| `S.list ~params ~row sql` | every row, in the order the database sent them |
| `S.exec_count ~params sql` | how many rows it changed |

```ocaml title="orders.ml"
module S = Rowtype

let ship = S.exec ~params:S.int "update orders set shipped = true where id = $1"

let count =
  S.find ~params:S.int ~row:S.int
    "select count(*) from orders where customer_id = $1"

let find =
  S.find_opt ~params:S.int
    ~row:S.(t2 int (opt text))
    "select id, note from orders where id = $1"

let of_customer =
  S.list ~params:S.int
    ~row:S.(t2 int int)
    "select id, total from orders where customer_id = $1 order by id"

let claim =
  S.exec_count
    ~params:S.(t2 text int)
    "update orders set owner = $1 where id = $2 and owner is null"
```

`S.find` is for a statement the database guarantees answers one row, an
`insert ... returning` or an aggregate. A lookup that may find nothing is
`S.find_opt`'s, and one that may match several rows is `S.list`'s, or a
`limit 1` says which. `S.exec` and `S.exec_count` discard any rows the
statement answers. A statement with no parameters takes `S.unit`.

## Running one

`Pg.run db statement args` runs it with its parameters and answers what it
says it answers:

```ocaml title="claim.ml"
module Pg = Rowtype_postgres

let claimed db ~owner order =
  Pg.run db Orders.claim (owner, order)
  |> Result.map (fun changed -> changed = 1)
```

A guarded write run as `S.exec_count` is an answer rather than a write:
`update ... where owner is null` asks "did I get it", decided by the
database in one statement, where a read and then a write is two.

A row with more columns or fewer than its shape reads is an error, as
[`verify`](checking-statements.md) refuses it before any run.

## Reading rows as they arrive

`Pg.fold db statement args ~init f` reads a `list` statement's rows as they
arrive, so a result of any size is read in the memory of one row:

```ocaml title="spent.ml"
module Pg = Rowtype_postgres

let spent db customer =
  Pg.fold db Orders.of_customer customer ~init:0 (fun sum (_id, total) ->
      sum + total)
```

Each row is decoded and handed to `f` as it arrives, as `List.fold_left`
would fold the list, without the list being made. A row that does not
decode is the answer, and `f` sees none after it.

## One statement, many values

`Pg.run_many db statement values` runs an `exec` statement once for each
value in one round trip, all or nothing: if any run fails, none applies.
No values sends nothing.

```ocaml title="ship_all.ml"
module Pg = Rowtype_postgres

let ship_all db ids = Pg.run_many db Orders.ship ids
```

[Bulk writes](bulk-writes.md) has the other ways to write
many rows.

## DDL and settings

`Pg.exec_raw db sql` runs statements with no parameters, several in one
call, and discards their rows: for DDL and a session's settings.

```ocaml title="session.ml"
module Pg = Rowtype_postgres

let set_up db =
  Pg.exec_raw db "set statement_timeout = '5s'; set application_name = 'shop'"
```

A transaction is never a `begin` and a `commit` sent this way. The backend
runs its own, in [`Pg.Transaction.within`](transactions.md),
since only it can tell a commit from a transaction Postgres rolled back in
silence.

## The same statements on every database

`rowtype` runs no statement itself. `Rowtype.Make` over a backend gives the
functions that run statements on that database's connection, `run`, `fold`,
`run_many` and `exec_raw`, and `Rowtype_postgres` and `Rowtype_sqlite` are
each one. A statement names no database, so the same value runs on both,
and one program may use two databases at once.

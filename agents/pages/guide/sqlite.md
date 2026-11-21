# SQLite

`rowtype-sqlite` runs the same statements on SQLite: the
[quick start's](../quick-start.md) `add_user` and `find_user` run on it
unchanged. Here they are in a module of their own:

```ocaml title="users.ml"
module S = Rowtype

type user = { id : int; name : string }

let user =
  S.map (S.t2 S.int S.text)
    ~parse:(fun (id, name) -> { id; name })
    ~print:(fun u -> (u.id, u.name))

let add_user =
  S.find ~params:S.text ~row:S.int
    "insert into users (name) values ($1) returning id"

let find_user =
  S.find_opt ~params:S.int ~row:user "select id, name from users where id = $1"
```

```ocaml title="sqlite.ml"
module Sq = Rowtype_sqlite

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let ( let* ) = Result.bind in
  match
    let* db =
      Sq.connect ~sw ~mono_clock:(Eio.Stdenv.mono_clock env) "shop.db"
    in
    let* () =
      Sq.exec_raw db
        "create table if not exists users (id integer primary key, name text \
         not null) strict"
    in
    let* id = Sq.run db Users.add_user "Ada" in
    Sq.run db Users.find_user id
  with
  | Ok (Some u) -> print_endline u.name
  | Ok None -> print_endline "no such user"
  | Error e ->
      prerr_endline (Sq.error_to_string e);
      exit 1
```

`Sq.connect ~sw ~mono_clock file` opens the database in `file`, making it
where there is none. `":memory:"` is a database in memory, the connection's
alone. A file that cannot be opened is `` `Unsent ``, and the connection is
closed when `sw` finishes, unless `Sq.close` closed it first.

## A C library

Unlike every other package here, it binds C: SQLite is a library, not a
server, so there is no protocol to speak in OCaml, and it links your
system's SQLite through [sqlite3](https://github.com/mmottl/sqlite3-ocaml).
SQLite's calls wait on the disk and on other connections' locks, so each
statement runs on a system thread and the domain's other fibers go on
meanwhile; its rows are read 64 at a time. `connect`'s `~timeout` is how
long a statement waits for another connection's lock, 5 s unless given,
after which it is `` `Not_serializable ``.

A fiber cancelled during a statement waits for the rows being read, then
the statement is ended and the cancellation passes. The connection is as
usable after it as before.

## Placeholders

A placeholder is `$1` as on Postgres, or SQLite's `?1` or `?`, each bound
by its number from the shape's values. One with a name, `:name`, is
refused, since a shape's values have none, and so are placeholders that do
not number the values from one, each at least once, before anything runs.
One statement runs at a time: a text holding several is `Sq.exec_raw`'s.

## How each column is kept

SQLite keeps four kinds of value, and each column is kept as one:

| Column | SQLite | Kept as |
|---|---|---|
| `S.int`, `S.int64` | integer | an `int` past 63 bits is refused |
| `S.float` | real | an integer is read too where a float holds it exactly; a NaN is NULL |
| `S.text`, `S.json`, `S.enum` | text | |
| `S.bytes` | blob | |
| `S.bool` | integer | `0` or `1` |
| `S.instant` | text | UTC, `2026-10-20 09:24:46.621000`: what `datetime()` reads and writes, so it sorts as time and compares with `datetime('now')`; read with any zone the text gives |
| `S.timestamp` | text | the same, and one with a zone is refused |
| `S.date` | text | `2026-10-20` |
| `S.interval` | text | `P14M3DT-0.000005S` |
| `S.uuid` | text | `7c9e6679-7425-40de-944b-e07fc1f90ae7`; a blob of 16 bytes is read too |
| `S.array` | text | a JSON array, which `json_each($1)` reads and `json_group_array` writes; `bytes` in it as hex |

Make tables `strict`, or declare each column `integer`, `real`, `text` or
`blob`. SQLite turns a value into its column's type where it can, and a
column of a type it does not know -- `uuid`, `json`, `timestamp` -- turns
text that looks like a number into one: a JSON document `123` comes back an
integer, which `S.json` refuses. A value of another kind than its scalar's
is refused, naming the kind and never the value.

## Failures

Each connection enforces foreign keys. A unique or primary key is
`` `Conflict (Some "users.email") ``, named by its columns as SQLite names
them, and a foreign key `` `Conflict None ``. A lock another connection still
holds once the wait is over -- 5 s unless `connect` is given `~timeout` --
is `` `Not_serializable ``: nothing was done, and trying again may succeed.

A statement SQLite cannot prepare fails in SQLite's words, which quote only
its text: `no such table: t`. One that fails as it runs is told by SQLite's
code alone, since a function quotes its argument in its message; a NOT NULL
or CHECK constraint keeps SQLite's words, which name the column or the
constraint and no value.

## Transactions

```ocaml title="sqlite_take.ml"
module S = Rowtype
module Sq = Rowtype_sqlite

let balance_of =
  S.find ~params:S.int ~row:S.int "select balance from accounts where id = $1"

let withdraw =
  S.exec
    ~params:S.(t2 int int)
    "update accounts set balance = balance - $2 where id = $1"

let ( let* ) = Result.bind

let take db account amount =
  Sq.Transaction.within db ~behaviour:Immediate ~retries:3 (fun db ->
      let* balance = Sq.run db balance_of account in
      if balance < amount then Error `Insufficient
      else Sq.run db withdraw (account, amount))
```

SQLite's transactions are serializable; what you choose is when one takes
the write lock, `Deferred` (SQLite's own), `Immediate` or `Exclusive`. A
transaction that reads and then writes begins `Immediate`, or its write can
find another connection wrote since it read. A statement that fails inside
one undoes itself alone, as SQLite does, and the transaction goes on. A
`COMMIT` SQLite refuses is rolled back and said, as on Postgres:
`` `Conflict `` for a deferred foreign key, `` `Not_serializable `` for a
lock it could not take, and `` `Not_committed `` for anything else.

`~retries` runs the whole transaction again when it meets
`` `Not_serializable ``, and `~keep` keeps what a refusal wrote, as on
[Postgres](transactions.md). Its `COMMIT` and each rollback
run to their end whatever cancels the fiber, so a cancellation never leaves
the transaction, or its lock, open.

`Sq.Transaction.savepoint` and `Sq.Transaction.release` keep the work done
between them in the transaction, and say `` `Gone `` where that work ended
the transaction itself: for a script you did not write, run between the two.

## A pool

A connection is one fiber's at a time. `Sq.Pool.create ~sw ~mono_clock
"shop.db"` opens connections to one file up front and `Sq.Pool.use` lends
them, each given back with no transaction open; for readers alongside a
writer, turn on WAL once, `pragma journal_mode = wal`, which the file keeps.

```ocaml title="sqlite_accounts.ml"
module S = Rowtype
module Sq = Rowtype_sqlite

let add_account =
  S.find
    ~params:S.(t2 text int)
    ~row:S.int
    "insert into accounts (email, balance) values ($1, $2) returning id"

let schema =
  "drop table if exists accounts; create table accounts (id integer primary \
   key, email text not null unique, balance integer not null) strict"

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let ( let* ) = Result.bind in
  match
    let* pool =
      Sq.Pool.create ~sw ~mono_clock:(Eio.Stdenv.mono_clock env) "accounts.db"
    in
    Result.join @@ Sq.Pool.use pool
    @@ fun db ->
    let* () = Sq.exec_raw db "pragma journal_mode = wal" in
    let* () = Sq.exec_raw db schema in
    let* () =
      Sq.verify db
        [
          S.Any add_account;
          S.Any Sqlite_take.balance_of;
          S.Any Sqlite_take.withdraw;
        ]
    in
    let* ada = Sq.run db add_account ("ada@example.com", 100) in
    (match Sq.run db add_account ("ada@example.com", 0) with
    | Error (`Conflict (Some columns)) -> Printf.printf "taken: %s\n" columns
    | Ok _ | Error _ ->
        prerr_endline "a second account took the same email";
        exit 1);
    let* () = Sqlite_take.take db ada 30 in
    Sq.run db Sqlite_take.balance_of ada
  with
  | Ok balance -> Printf.printf "balance: %d\n" balance
  | Error `Insufficient ->
      prerr_endline "insufficient";
      exit 1
  | Error (#Sq.error as e) ->
      prerr_endline (Sq.error_to_string e);
      exit 1
```

| Option | Default | |
|---|---|---|
| `~size` | 8 | connections, at least one |
| `~wait` | 5 s | how long a borrow waits for a connection, after which it is `` `Pool `Busy `` |
| `~timeout` | 5 s | each connection's wait for another's lock |

A database in memory is refused, since each connection would have one of
its own: a pool is of a file. A connection a borrower left in a transaction
is rolled back before it is lent again.

## Checking and loading

`Sq.verify db statements` prepares every statement without running it, as
[checking statements](checking-statements.md#on-sqlite) says.
`Sq.copy_in` loads rows of any number from a `Seq.t` with one prepared
`insert`, all or none, as `Pg.copy_in` does with `COPY`.

Each statement is a `debug` line on the `rowtype-sqlite` log source, with
how long it took: its first line of text, never its parameters.

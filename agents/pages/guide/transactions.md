# Transactions

A transaction is the backend's, since each database runs one its own way:
`rowtype` runs none, and `Pg.Transaction.within` is the Postgres backend's.
SQLite's is [on its own page](sqlite.md#transactions).

## Within a transaction

```ocaml title="take.ml"
module S = Rowtype
module Pg = Rowtype_postgres

let balance_of =
  S.find ~params:S.int ~row:S.int "select balance from accounts where id = $1"

let withdraw =
  S.exec
    ~params:S.(t2 int int)
    "update accounts set balance = balance - $2 where id = $1"

let ( let* ) = Result.bind

let take db account amount =
  Pg.Transaction.within db ~isolation:Serializable ~retries:3 (fun db ->
      let* balance = Pg.run db balance_of account in
      if balance < amount then Error `Insufficient
      else Pg.run db withdraw (account, amount))
```

`Ok` commits and `Error` rolls back. A raise inside the work rolls back and
passes.

| Option | Default | |
|---|---|---|
| `~isolation` | the database's own, `Read_committed` in a Postgres nobody configured | `Read_committed`, `Repeatable_read` or `Serializable` |
| `~retries` | none | how many times the whole transaction runs again when it meets `` `Not_serializable `` |
| `~keep` | nothing | which refusals commit what they wrote anyway |

Work that runs again does everything again, so work given `~retries` does
nothing but its statements: a message sent or a counter moved inside it
happens once for every attempt. Each retry is an `info` line, and the last
answer stands.

`~keep` is for the rarer refusal that has to keep what it wrote -- a wrong
sign-in code that counts its attempt -- and still answers the refusal.

## The transaction's own failures

A transaction that could not commit is `` `Not_committed ``, never a silent
success, and one whose `COMMIT` was sent with no answer back is `` `Lost ``:
it may have committed. A `DEFERRABLE` constraint that refuses the write at
`COMMIT` is `` `Conflict ``, named, as it is when a statement finds it.
`` `Unsent `` is a transaction that never began, on a connection that could
not be made again, so nothing of the work was sent.

`` `Not_committed `` is also the answer when the work answered `Ok` after a
failed statement aborted the transaction -- an error the work swallowed --
since Postgres rolls such a transaction back in silence. A `within` inside
another is `` `Not_committed `` before anything begins, and the outer one
goes on. Each `` `Not_committed `` and `` `Lost `` is an `error` line on the
`rowtype-postgres` log source.

The transaction's own failures are `Pg.Transaction.failure`, so a caller
whose errors are an ordinary variant tells them from its own:

```ocaml title="members.ml"
module S = Rowtype
module Pg = Rowtype_postgres

let add_member =
  S.find ~params:S.text ~row:S.int
    "insert into members (name) values ($1) returning id"

type error =
  | Empty_name
  | Taken of string option
  | Failed of string
  | Unavailable of Pg.Transaction.failure

let work db name =
  if String.equal name "" then Error Empty_name
  else
    match Pg.run db add_member name with
    | Ok id -> Ok id
    | Error (`Conflict name) -> Error (Taken name)
    | Error e -> Error (Failed (S.error_to_string e))

let join db name =
  match
    Pg.Transaction.within db (fun db ->
        Result.map_error (fun e -> `Work e) (work db name))
  with
  | Ok v -> Ok v
  | Error (`Work e) -> Error e
  | Error (`Conflict name) -> Error (Taken name)
  | Error (#Pg.Transaction.failure as f) -> Error (Unavailable f)
```

A caller whose errors are polymorphic variants, as every statement's are,
writes nothing: the transaction's join the work's in one result.

## A connection the server dropped

`within` makes again a connection the server dropped before it begins, and
begins once more if the first `begin` fails, the one point where starting
again loses nothing. A fiber cancelled inside the work closes the
connection, which ends the transaction with nothing kept, and the
cancellation passes; the next `within` on it makes it again.

# rowtype

[![ci](https://img.shields.io/github/actions/workflow/status/hyphatech/rowtype/ci.yml?branch=main&label=ci)](https://github.com/hyphatech/rowtype/actions/workflows/ci.yml)
[![release](https://img.shields.io/github/v/release/hyphatech/rowtype?label=release)](https://github.com/hyphatech/rowtype/releases)
[![license](https://img.shields.io/github/license/hyphatech/rowtype)](LICENSE)
![OCaml 5.4+](https://img.shields.io/badge/OCaml-5.4%2B-EC6813?logo=ocaml&logoColor=white)

Typed Postgres queries and migrations for OCaml 5.

You write the SQL. You declare its parameters and rows once, and that one
declaration binds the values going in and decodes the rows coming out. No
ORM, no query DSL, no ppx.

## Why not Caqti?

Caqti is the mature choice, and the right one for MariaDB, SQLite or Lwt.
rowtype does less, Postgres on Eio only, and in return gives you:

- a driver written for Eio, [postgres-eio](https://github.com/hyphatech/postgres-eio),
  rather than libpq bindings or a driver adapted to it;
- every statement checked against your database, from a test, without
  running it;
- migrations in the box.

## Install

```sh
opam pin add postgres-eio https://github.com/hyphatech/postgres-eio.git
opam pin add https://github.com/hyphatech/rowtype.git
```

```lisp
(libraries rowtype rowtype-postgres eio_main)
```

## Quick start

```ocaml
module S = Rowtype
module Pg = Rowtype_postgres

type user = { id : int; name : string }

let user =
  S.conv (S.t2 S.int S.text)
    ~of_:(fun (id, name) -> { id; name })
    ~to_:(fun u -> (u.id, u.name))

let add_user =
  S.find ~params:S.text ~row:S.int
    "insert into users (name) values ($1) returning id"

let find_user =
  S.find_opt ~params:S.int ~row:user "select id, name from users where id = $1"

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let ( let* ) = Result.bind in
  match
    let* conninfo = Pg.conninfo "postgres://app@localhost/shop" in
    let* db =
      Pg.connect ~sw ~net:(Eio.Stdenv.net env)
        ~mono_clock:(Eio.Stdenv.mono_clock env) conninfo
    in
    let* id = Pg.run db add_user "Ada" in
    Pg.run db find_user id
  with
  | Ok (Some u) -> print_endline u.name
  | Ok None -> print_endline "no such user"
  | Error e -> prerr_endline (S.error_to_string e)
```

A statement says how many rows it answers:

| Statement | Answers |
|---|---|
| `S.exec ~params sql` | `unit` |
| `S.find ~params ~row sql` | exactly one row; none or several is an error |
| `S.find_opt ~params ~row sql` | at most one row; several is an error |
| `S.list ~params ~row sql` | every row |
| `S.exec_count ~params sql` | how many rows it changed |

`Pg.fold db statement args ~init f` reads a `list` statement's rows as they
arrive, so a result of any size is read in the memory of one row.

| Column | Postgres | OCaml |
|---|---|---|
| `S.int` | `int2`, `int4`, `int8`, `oid` | `int`; an `int8` past 63 bits is refused |
| `S.float` | `float4`, `float8` | `float` |
| `S.text` | `text`, `varchar`, `char`, `name`, an enum's label | `string` |
| `S.bytes` | `bytea` | `string` |
| `S.bool` | `bool` | `bool` |
| `S.instant` | `timestamptz` | `Ptime.t`, kept to the microsecond |
| `S.uuid` | `uuid` | `Uuidm.t` |
| `S.json` | `json`, `jsonb` | `string`, the document |

`S.opt` is a NULL, `S.array` a Postgres array, and `S.conv` or `S.parse` map
a column onto your own type. A list bound as an array writes many rows in one
statement and one round trip:

```ocaml
let add_users =
  S.exec ~params:S.(t2 (array text) (array text))
    "insert into users (name, email) select * from unnest($1::text[], $2::text[])"
```

`Pg.copy_in` loads rows of any number with one `COPY`, each bound by its
shape as a statement's parameters are, and read from a `Seq.t` as they are
sent, so a load is held a row at a time:

```ocaml
Pg.copy_in db ~table:"users" ~columns:[ "name"; "email" ]
  S.(t2 text text) (Seq.of_list users)
```

## Errors and transactions

Every failure is a value. `` `Conflict (Some "users_email_key") `` is a
unique, exclusion or foreign-key constraint that refused the write, named.
A NOT NULL or CHECK violation is the program's mistake, so it is `` `Db ``.
`` `Not_serializable `` is a
transaction a concurrent one won. `` `Db `` is anything else. They are
polymorphic variants, so they join your own errors in one result.

```ocaml
Pg.Transaction.within db ~isolation:Serializable ~retries:3 (fun db ->
    let* balance = Pg.run db balance_of account in
    if balance < amount then Error `Insufficient
    else Pg.run db withdraw (account, amount))
```

`Ok` commits and `Error` rolls back. A transaction that could not commit is
`` `Not_committed ``, never a silent success.

## Checking statements

`Pg.verify db statements` has Postgres describe every statement without
running it, and compares the parameters and columns with what you declared.
Run it in a test against a migrated database, and a renamed column fails the
test instead of a request.

## Migrations

Plain SQL files named `<UTC stamp>_<name>.sql`, forward only, applied by a
command of their own before your application starts:

```sh
rowtype-migrate create             # make the database
rowtype-migrate new add_users      # migrations/<UTC stamp>_add_users.sql
rowtype-migrate up                 # apply what is pending
rowtype-migrate status [--check]   # applied and pending
rowtype-migrate dump [--check]     # the schema they make, as db/schema.sql
rowtype-migrate squash VERSION     # every migration through it, as one baseline
```

`up` applies each file in its own transaction, under an advisory lock, and
records a digest of it. A migration edited after it ran, or merged out of
order, is refused. `squash` proves the baseline makes the same schema as the
history before it changes a file. The database is `--url`, or
`DATABASE_URL`.

## Packages

| Package | What it is |
|---|---|
| `rowtype` | queries described once; links no database library |
| `rowtype-postgres` | the Postgres backend, over [postgres-eio](https://github.com/hyphatech/postgres-eio) |
| `rowtype-migrate` | the migrations, and the `rowtype-migrate` command |

Each module's `.mli` is its reference.

## Contributing

See [AGENTS.md](AGENTS.md).

## Licence

MIT, copyright Hypha Technologies Ltd. See [LICENSE](LICENSE).

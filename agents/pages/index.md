# Getting Started

SQL queries and migrations for modern OCaml.

rowtype runs plain SQL from OCaml. A statement is written as the database
reads it, and its parameters and its rows are described once, as OCaml
values. The same statements run on Postgres and on SQLite. A test can ask
the database to describe each one without running it, so a renamed column
fails a test instead of a request. Migrations are plain SQL files, applied
by one command for either database.

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

## Install

It needs opam, with a switch of OCaml 5.5 or later.

```sh
opam repo add open-opam https://github.com/open-opam/opam-repository.git
opam install rowtype rowtype-postgres rowtype-migrate   # Postgres
opam install rowtype rowtype-sqlite rowtype-migrate     # SQLite, through its C library
```

| Package | What it is |
|---|---|
| `rowtype` | queries described once; links no database library |
| `rowtype-postgres` | the Postgres backend, over [postgres-eio](https://hyphatech.github.io/postgres-eio/) |
| `rowtype-sqlite` | the SQLite backend, over the [sqlite3](https://github.com/mmottl/sqlite3-ocaml) binding |
| `rowtype-migrate` | the migrations and the `rowtype-migrate` command, for each dialect installed beside it |
| `rowtype-test` | a Postgres database for a test run, in Podman or Docker, and the `rowtype-test` command |

`rowtype-migrate` migrates the dialects whose backend is installed beside
it, and links no other: its libraries are `rowtype-migrate.postgres` and
`rowtype-migrate.sqlite`, each built where its backend is.

`rowtype-sqlite` links your system's SQLite. Where it is missing, opam
offers to install it with your system's package manager.

## Name the libraries

A program names the libraries it uses in its `dune` file. On Postgres:

```lisp
(libraries rowtype rowtype-postgres eio_main)
```

On SQLite:

```lisp
(libraries rowtype rowtype-sqlite eio_main)
```

The [quick start](quick-start.md) writes a whole program, next.

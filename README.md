# rowtype

[![ci](https://img.shields.io/github/actions/workflow/status/hyphatech/rowtype/ci.yml?branch=main&label=ci)](https://github.com/hyphatech/rowtype/actions/workflows/ci.yml)
[![release](https://img.shields.io/github/v/release/hyphatech/rowtype?label=release)](https://github.com/hyphatech/rowtype/releases)
[![license](https://img.shields.io/github/license/hyphatech/rowtype)](LICENSE)
![OCaml 5.5+](https://img.shields.io/badge/OCaml-5.5%2B-EC6813?logo=ocaml&logoColor=white)
[![Hypha Stack](https://img.shields.io/badge/Hypha-Stack-d4a820?labelColor=15120e)](https://hyphatech.io/stack/)

SQL queries and migrations for modern OCaml.
Plain SQL, checked against your database. Postgres and SQLite alike.

**Documentation**: [hyphatech.github.io/rowtype](https://hyphatech.github.io/rowtype/)

## Features

- **Plain SQL**: no ORM, no query DSL, no ppx.
- **Checked against your database**: every statement described by the database from a test, without running it, so a renamed column fails a test instead of a request.
- **Postgres and SQLite**: the same statements run on both.
- **Migrations in the box**: plain SQL files and one command for every dialect, a library's own migrations included.
- **Failures you can act on**: a refused write names its constraint, and a lost write is told from one never sent.
- **Any size**: rows read in the memory of one, and loads through COPY.

## Install

```sh
opam repo add hypha https://github.com/hyphatech/opam-repository.git
opam install rowtype rowtype-postgres rowtype-migrate   # Postgres
opam install rowtype rowtype-sqlite rowtype-migrate     # SQLite, through its C library
```

Add to your `dune`:

```lisp
(libraries rowtype rowtype-postgres eio_main)
```

## Example

```ocaml
module S = Rowtype
module Pg = Rowtype_postgres

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

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let ( let* ) = Result.bind in
  match Pg.Conninfo.of_string "postgres://app@localhost/shop" with
  | Error m -> prerr_endline m
  | Ok target -> (
      match
        let* db =
          Pg.connect ~sw ~net:(Eio.Stdenv.net env)
            ~mono_clock:(Eio.Stdenv.mono_clock env)
            target
        in
        let* id = Pg.run db add_user "Ada" in
        Pg.run db find_user id
      with
      | Ok (Some u) -> print_endline u.name
      | Ok None -> print_endline "no such user"
      | Error e -> prerr_endline (S.error_to_string e))
```

The same statements run on SQLite unchanged: [SQLite](https://hyphatech.github.io/rowtype/guide/sqlite/).

## Contributing

See [AGENTS.md](AGENTS.md).

## Licence

MIT, copyright Hypha Technologies Ltd. See [LICENSE](LICENSE).

# Quick start

A program that adds a user to Postgres and finds it again by the id
Postgres gave it.

## The table

It needs a Postgres it reaches as `postgres://app@localhost/shop`, holding
one table:

```sql
create table users (
  id bigint generated always as identity primary key,
  name text not null
);
```

## The program

In a directory of its own, a `dune-project`:

```lisp
(lang dune 3.18)
```

a `dune`:

```lisp
(executable
 (name main)
 (libraries rowtype rowtype-postgres eio_main))
```

and `main.ml`:

```ocaml title="main.ml"
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

```console
$ dune exec ./main.exe
Ada
```

## What it does

`user`, `add_user` and `find_user` are declared once, before anything
connects. `add_user` takes a `text` and answers one `int`, the id;
`find_user` takes an `int` and answers at most one `user`, read through
`S.map` from the row's two columns. `Pg.run` binds the values going in and
decodes the rows coming out, so `id` is an `int` and `u` a `user`, with no
conversion written by hand.

Every failure is a value, here a `result`. `S.error_to_string` words one for
a log, and never holds a parameter's value.

## Connecting

A connection is made to a `Pg.Conninfo.t`, read once from a connection
string, a URL or a keyword list; `Pg.Conninfo.with_database` names another
database on the same server, and `Pg.Conninfo.with_connect_timeout` bounds
each address it tries. Connection strings are
[postgres-eio's](https://hyphatech.github.io/postgres-eio/), the driver
under `rowtype-postgres`.

A program serving requests borrows its connections from a
[pool](guide/pools.md). The same statements run on
[SQLite](guide/sqlite.md) unchanged.

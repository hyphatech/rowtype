# Pools

A connection is one fiber's at a time. A program serving requests makes a
pool once, where it starts, and lends its connections to each piece of
work. `rowtype-postgres` makes connections and pools as
[postgres-eio](https://hyphatech.github.io/postgres-eio/) makes them,
every option passed through.

## A pool

```ocaml title="pool.ml"
module S = Rowtype
module Pg = Rowtype_postgres

let count_users = S.find ~params:S.unit ~row:S.int "select count(*) from users"

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  match Pg.Conninfo.of_string "postgres://app@localhost/shop" with
  | Error m -> prerr_endline m
  | Ok target -> (
      let ( let* ) = Result.bind in
      match
        let* pool =
          Pg.Pool.create ~sw ~net:(Eio.Stdenv.net env)
            ~mono_clock:(Eio.Stdenv.mono_clock env)
            ~size:16 ~observe:Counting.observe target
        in
        let users =
          Result.join (Pg.Pool.use pool (fun db -> Pg.run db count_users ()))
        in
        Pg.Pool.close pool;
        users
      with
      | Ok n ->
          Printf.printf "%d users, in %d statements\n" n
            (Atomic.get Counting.statements)
      | Error e -> prerr_endline (Pg.error_to_string e))
```

`Pg.Pool.create` opens every connection up front, and answers the first
that could not be made, closing those made before it.

| Option | Default | |
|---|---|---|
| `~size` | 8 | connections, at least one |
| `~wait` | 5 s | how long a borrow waits for a connection |
| `~reset` | `true` | each connection's session reset before it is lent again |
| `~max_lifetime` | 30 min | a connection older than this is replaced |
| `~idle_check` | 60 s | a connection idle longer is pinged, and replaced if it fails |
| `~timeout` | 30 s | the longest a connection waits on the server for bytes to move |
| `~observe` | none | an [observer](#watching-statements) of every statement its connections run |

The durations are `Mtime.Span.t`s, written `Mtime.Span.(2 * s)`. A
connection's reset ends what a borrower left in its session -- an open
transaction, a `LISTEN`, its settings -- and keeps the prepared statements
borrowers share; `~reset:false` keeps the settings.

`Pg.Pool.use pool f` borrows a connection, runs `f` on the calling fiber,
and gives it back, even if `f` raises. It is `` `Pool `Busy `` if no
connection came free within the wait, and `` `Pool `Closed `` at once if the
pool is closed, or closes while the borrow waits. `Pg.Pool.stats` says how
many connections it keeps, how many are idle, how many borrows wait and how
many it replaced, for a health endpoint. `Pg.Pool.close` closes it.

A transaction in a borrowed connection is one result:

```ocaml
Result.join (Pg.Pool.use pool (fun db -> Pg.Transaction.within db work))
```

## One connection

`Pg.connect ~sw ~net ~mono_clock target` makes one connection, with the
same `?timeout` and `?observe`, `?parameters` for the session's settings,
sent as it starts (`("statement_timeout", "10s")`), and `?statement_cache`
for how many statements it keeps parsed, 256 unless given; `0` turns the
cache off, for a pooler without named statements.

- `Pg.close db` closes it for good, telling the server; its next statement
  says so.
- `Pg.revive db` makes again, in place, a connection a failure closed --
  the server restarted, say -- and is `Ok` at once for an open one. One
  `close` closed stays closed.
- `Pg.timeout db` and `Pg.set_timeout db ~timeout` read and set the bound on
  every read and write, lifted around work that runs longer, a migration,
  and put back after it.

A statement runs on the calling fiber. A fiber cancelled with a statement
in flight closes its connection and asks the server to cancel the
statement, so nothing runs on for nobody. A raise from `Pg.fold`'s function
passes, and closes the connection, whose answer was read only in part: its
next statement is `` `Connection `Closed `` until `Pg.revive`.

## Watching statements

An observer is handed each statement's text as it runs, and runs it -- a
span begun and ended around it, a count:

```ocaml title="counting.ml"
module Pg = Rowtype_postgres

let statements = Atomic.make 0

let observe : Pg.observer =
  {
    around =
      (fun _sql run ->
        Atomic.incr statements;
        run ());
  }
```

The text has placeholders where the values are, and the values are never
handed over, since a code, a digest or a token is one. It watches a
transaction's `begin` and `commit` too. Each statement is also a `debug`
line on the `rowtype-postgres` log source, with how long it took: its first
line of text, never its parameters.

## Notifications

A session that ran `LISTEN` keeps what it hears, so a listener is a
connection of its own, never a pooled one:

```ocaml title="listener.ml"
module Pg = Rowtype_postgres

let ( let* ) = Result.bind

let rec hear listener =
  let* event = Pg.Listener.next listener in
  (match (event : Pg.Listener.event) with
  | Notification { channel; payload } ->
      Printf.printf "%s: %s\n%!" channel payload
  | Reconnected -> print_endline "reconnected: what was sent meanwhile is gone");
  hear listener

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  match Pg.Conninfo.of_string "postgres://app@localhost/shop" with
  | Error m -> prerr_endline m
  | Ok target -> (
      match
        let* listener =
          Pg.Listener.connect ~sw ~net:(Eio.Stdenv.net env)
            ~mono_clock:(Eio.Stdenv.mono_clock env)
            target
        in
        let* () = Pg.Listener.listen listener "orders" in
        hear listener
      with
      | Ok () -> ()
      | Error e -> prerr_endline (Pg.error_to_string e))
```

`Pg.Listener.next` answers the next notification, or waits for one. A lost
connection is made again, every channel listened to again, and said so:
`Reconnected`, since whatever was sent in between is gone. `~keep_alive` is
how long `next` waits in silence before it pings the server, 10 s unless
given. `Pg.Listener.unlisten` stops hearing a channel, and
`Pg.Listener.close` closes the listener for good.

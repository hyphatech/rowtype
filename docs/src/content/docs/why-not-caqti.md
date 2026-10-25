---
title: Why not Caqti?
---

Caqti is the mature choice, and the right one for MariaDB or Lwt. The
rowtype library does less, Postgres and SQLite on Eio only, and in return
gives you:

- a driver written for Eio, [postgres-eio](https://hyphatech.github.io/postgres-eio/),
  rather than libpq bindings or a driver adapted to it;
- every statement [checked against your database](/rowtype/guide/checking-statements/),
  from a test, without running it;
- [migrations](/rowtype/guide/migrations/) in the box, the same files and
  command for every dialect.

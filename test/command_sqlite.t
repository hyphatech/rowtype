The command on SQLite.

  $ unset DATABASE_URL
  $ rowtype-migrate new add_users --dir m > /dev/null

SQLite needs no server, so the whole of a database's life runs here: made,
migrated, read, dumped, squashed and dropped by its file.

  $ rowtype-migrate status --dir m --url sqlite:app.db
  there is no database "app.db": make it with `rowtype-migrate create`
  [1]
  $ rowtype-migrate create --url sqlite:app.db
  $ rowtype-migrate create --url sqlite:app.db
  the database app.db exists already
  [1]
  $ cat > m/20260101000000_users.sql <<'END'
  > create table users (id integer primary key, name text not null);
  > insert into users (name) values ('ada');
  > END
  $ rowtype-migrate status --dir m --url sqlite:app.db | sed -E 's/[0-9]{14}_add/STAMP_add/'
  pending  20260101000000_users
  pending  STAMP_add_users
Postgres's flags are refused on a SQLite file, which has no advisory lock and
no pg_dump, before anything is done:

  $ rowtype-migrate up --dir m --url sqlite:app.db --lock 5
  --lock is Postgres's advisory lock, and a SQLite file has none: leave it out
  [1]
  $ rowtype-migrate dump --dir m --url sqlite:app.db --pg-dump pg_dump
  --pg-dump is Postgres's, and a SQLite dump runs no program: leave it out
  [1]
  $ rowtype-migrate squash 20260101000000 --dir m --url sqlite:app.db --restrict-key k
  --restrict-key is pg_dump's, and a SQLite dump runs no program: leave it out
  [1]
  $ rowtype-migrate up --dir m --url sqlite:app.db
  $ rowtype-migrate status --dir m --url sqlite:app.db --check | sed -E 's/[0-9]{14}_add/STAMP_add/'
  applied  20260101000000_users
  applied  STAMP_add_users
  $ rowtype-migrate dump --dir m --url sqlite:app.db -o schema.sql
  schema.sql
  $ cat schema.sql
  CREATE TABLE schema_migrations (
    source text not null default 'app',
    version integer not null,
    name text not null,
    applied_at text not null default (strftime('%Y-%m-%d %H:%M:%f', 'now')),
    checksum text,
    primary key (source, version)
  );
  CREATE TABLE users (id integer primary key, name text not null);
  $ rowtype-migrate dump --dir m --url sqlite:app.db -o schema.sql --check
  $ rowtype-migrate squash 20260101000000 --dir m --url sqlite:app.db
  m/20260101000000_baseline.sql, in place of 1 migration
  $ cat m/20260101000000_baseline.sql
  -- rowtype-migrate: baseline
  -- Every migration through 20260101000000, as the database they made: written by
  -- `rowtype-migrate squash`, and never edited.
  pragma defer_foreign_keys = on;
  CREATE TABLE users (id integer primary key, name text not null);
  insert into "users" values (1, 'ada');
  $ rowtype-migrate up --dir m --url sqlite:app.db
  $ rowtype-migrate drop --url sqlite:app.db
  $ test -e app.db
  [1]

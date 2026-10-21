The command, where it needs no database and no dialect.

  $ unset DATABASE_URL

A new migration is stamped in UTC and made where it is asked for:

  $ rowtype-migrate new add_users --dir m | sed -E 's/[0-9]{14}/STAMP/'
  m/STAMP_add_users.sql
  $ rowtype-migrate new Add-Users --dir m
  "Add-Users" is not a migration name: lower-case letters, digits and underscores
  [1]

A database is --url, or else DATABASE_URL, or else the variable --env
names, from --env-file where one is given:

  $ rowtype-migrate status --dir m
  no database: give --url, or set DATABASE_URL
  [1]
  $ echo 'DATABASE_URL=x' > .env
  $ rowtype-migrate status --dir m --env-file .env --env OTHER
  .env does not set OTHER
  [1]

A squash is through a migration's version, fourteen digits, refused before
anything is read:

  $ rowtype-migrate squash 7 --dir m --url sqlite:app.db
  7 is not a migration's version: fourteen digits
  [1]


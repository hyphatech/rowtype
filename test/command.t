The command, where it needs no database. A connection string here is a
key the driver refuses, so the failure names which one was read.

  $ unset DATABASE_URL

A new migration is stamped in UTC and made where it is asked for:

  $ rowtype-migrate new add_users --dir m | sed -E 's/[0-9]{14}/STAMP/'
  m/STAMP_add_users.sql
  $ rowtype-migrate new Add-Users --dir m
  "Add-Users" is not a migration name: lower-case letters, digits and underscores
  [1]

The database is --url, or else DATABASE_URL, or else the variable --env
names; from --env-file where one is given, the last line that sets it:

  $ rowtype-migrate status --dir m
  no database: give --url, or set DATABASE_URL
  [1]
  $ DATABASE_URL="from_environment=1" rowtype-migrate status --dir m
  the connection string's "from_environment" is not a key this driver reads
  [1]
  $ OTHER="named=1" rowtype-migrate status --dir m --env OTHER
  the connection string's "named" is not a key this driver reads
  [1]
  $ cat > .env <<'END'
  > # a comment, and a blank line
  > 
  > export DATABASE_URL="first=1"
  > DATABASE_URL='last=1'
  > END
  $ DATABASE_URL="from_environment=1" rowtype-migrate status --dir m --env-file .env
  the connection string's "last" is not a key this driver reads
  [1]
  $ rowtype-migrate status --dir m --env-file .env --url "given=1"
  the connection string's "given" is not a key this driver reads
  [1]
  $ rowtype-migrate status --dir m --env-file .env --env OTHER
  .env does not set OTHER
  [1]

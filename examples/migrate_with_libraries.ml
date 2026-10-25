let migrate env ~prefix target =
  let ( let* ) = Result.bind in
  let* spool = Rowtype_migrate.of_package ~prefix "spool" in
  let* spool = Rowtype_migrate.source "spool" spool in
  let* migrations = Rowtype_migrate.of_directory "migrations" in
  Rowtype_migrate_postgres.run env target @@ fun db ->
  Rowtype_migrate_postgres.apply ~libraries:[ spool ] db migrations

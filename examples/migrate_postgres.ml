let migrate env target =
  let ( let* ) = Result.bind in
  let* migrations = Rowtype_migrate.of_directory "migrations" in
  Rowtype_migrate_postgres.run env target @@ fun db ->
  Rowtype_migrate_postgres.apply db migrations

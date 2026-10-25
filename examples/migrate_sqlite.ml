let migrate env file =
  let ( let* ) = Result.bind in
  let* migrations = Rowtype_migrate.of_directory "migrations" in
  Rowtype_migrate_sqlite.run env file @@ fun db ->
  Rowtype_migrate_sqlite.apply db migrations

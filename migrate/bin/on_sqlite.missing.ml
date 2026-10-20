(* SQLite, where rowtype-sqlite is not installed: a sqlite: URL is said to
   need it. *)

let database url =
  Option.map
    (fun _ ->
      Error
        "this rowtype-migrate was built without SQLite: install \
         rowtype-sqlite, and opam builds it again with it")
    (Database.sqlite_file url)

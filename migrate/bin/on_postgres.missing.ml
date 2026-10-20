(* Postgres, where rowtype-postgres is not installed: any URL that is not
   another dialect's is said to need it. *)

let database url =
  match Database.sqlite_file url with
  | Some _ -> None
  | None ->
      Some
        (Error
           "this rowtype-migrate was built without Postgres: install \
            rowtype-postgres, and opam builds it again with it")

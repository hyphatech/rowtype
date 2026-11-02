(* A suite's Postgres: the server its runner hands -- rowtype-test run
   hands one for a whole run -- or one of its own in Podman, removed as the
   switch ends; the suite's migrations run once into a template; and a copy
   of it for each case, dropped after. *)

let run_suite env ~migrations cases =
  Eio.Switch.run @@ fun sw ->
  let migrate conninfo =
    Rowtype_migrate_postgres.run env conninfo (fun db ->
        Rowtype_migrate_postgres.apply db migrations)
  in
  match
    Result.bind
      (Rowtype_test.server ~sw ?url:(Sys.getenv_opt "TEST_DATABASE_URL") env)
      (Rowtype_test.template ~setup:migrate)
  with
  | Error (#Rowtype_test.error as e) -> Error (Rowtype_test.error_to_string e)
  | Error (#Rowtype_migrate.error as e) ->
      Error (Rowtype_migrate.error_to_string e)
  | Ok template ->
      Ok
        (List.map
           (fun case ->
             Rowtype_test.database template (fun conninfo -> case env conninfo))
           cases)

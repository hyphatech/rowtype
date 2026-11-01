(* A suite's Postgres: the server its runner names, or one of its own in
   Podman, removed as the switch ends; and a database of each case's own in
   it, dropped after. *)

let run_suite env cases =
  Eio.Switch.run @@ fun sw ->
  match
    Rowtype_test.server ~sw ?url:(Sys.getenv_opt "TEST_DATABASE_URL") env
  with
  | Error e -> Error (Rowtype_test.error_to_string e)
  | Ok server ->
      Ok
        (List.map
           (fun case ->
             Rowtype_test.database server (fun conninfo -> case env conninfo))
           cases)

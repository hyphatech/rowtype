(* rowtype-test run -- COMMAND: one Postgres for a whole test run, as a
   pytest session fixture is -- started, handed to the command and every
   process it runs as TEST_DATABASE_URL, and removed when the command ends,
   whatever its result. Each suite the command runs makes its databases in
   it, so a run starts one container, not one a suite. A run killed while
   its container ran leaves it to the next run, which removes it. *)

let usage = "usage: rowtype-test run -- COMMAND [ARGUMENT...]"
let variable = "TEST_DATABASE_URL"

let () =
  match Array.to_list Sys.argv with
  | _ :: "run" :: "--" :: (_ :: _ as command) ->
      let code =
        Eio_main.run @@ fun env ->
        Eio.Switch.run @@ fun sw ->
        (* A server handed already is the run's: nothing is started. *)
        match Rowtype_test.server ~sw ?url:(Sys.getenv_opt variable) env with
        | Error e ->
            prerr_endline ("rowtype-test: " ^ Rowtype_test.error_to_string e);
            2
        | Ok server -> (
            let url =
              Rowtype_postgres.Conninfo.to_url (Rowtype_test.conninfo server)
            in
            let environment =
              Array.append
                [| variable ^ "=" ^ url |]
                (Array.of_list
                   (List.filter
                      (fun kv ->
                        not (String.starts_with ~prefix:(variable ^ "=") kv))
                      (Array.to_list (Unix.environment ()))))
            in
            match
              Eio.Process.await
                (Eio.Process.spawn ~sw
                   (Eio.Stdenv.process_mgr env)
                   ~env:environment command)
            with
            | `Exited code -> code
            | `Signaled signal -> 128 + abs signal
            | exception Eio.Io (e, _) ->
                prerr_endline
                  (Format.asprintf "rowtype-test: %a" Eio.Exn.pp_err e);
                127)
      in
      exit code
  | _ ->
      prerr_endline usage;
      exit 2

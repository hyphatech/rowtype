(* rowtype-test run -- COMMAND: one Postgres for a whole test run, as a
   pytest session fixture is -- started, handed to the command and every
   process it runs as TEST_DATABASE_URL, and removed when the command ends,
   whatever its result. Each suite the command runs makes its databases in
   it, so a run starts one container, not one a suite. A run killed while
   its container ran leaves it to the next run, which removes it. *)

let usage =
  "usage: rowtype-test run [--runtime podman|docker] -- COMMAND [ARGUMENT...]"

let variable = "TEST_DATABASE_URL"

(* The runtime is named or Podman's, and a word but these two is the
   usage, not a binary looked for. *)
let parse = function
  | "run" :: "--" :: (_ :: _ as command)
  | "run" :: "--runtime" :: "podman" :: "--" :: (_ :: _ as command) ->
      Some (`Podman, command)
  | "run" :: "--runtime" :: "docker" :: "--" :: (_ :: _ as command) ->
      Some (`Docker, command)
  | _ -> None

let () =
  match parse (List.drop 1 (Array.to_list Sys.argv)) with
  | Some (runtime, command) ->
      (* The container this run could not remove is said on stderr. *)
      Logs.set_reporter (Logs.format_reporter ());
      let code =
        Eio_main.run @@ fun env ->
        Eio.Switch.run @@ fun sw ->
        (* A server handed already is the run's: nothing is started, and
           it is handed on as it came. *)
        let handed = Sys.getenv_opt variable in
        match Rowtype_test.server ~sw ?url:handed ~runtime env with
        | Error e ->
            prerr_endline ("rowtype-test: " ^ Rowtype_test.error_to_string e);
            2
        | Ok server -> (
            let url =
              match handed with
              | Some url -> url
              | None ->
                  Rowtype_postgres.Conninfo.to_url
                    (Rowtype_test.conninfo server)
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
            (* Eio names a signal by OCaml's number; a shell's status is
               the system's. *)
            | `Signaled signal -> 128 + Sys.signal_to_int signal
            | exception Eio.Io (e, _) ->
                prerr_endline
                  (Format.asprintf "rowtype-test: %a" Eio.Exn.pp_err e);
                127)
      in
      exit code
  | None ->
      prerr_endline usage;
      exit 2

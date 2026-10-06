(* rowtype-migrate: the command a person, a deployment or a build runs
   against a database's migrations. It links no application, so migrating is
   a step of its own, run before the build that needs it starts. *)

module M = Rowtype_migrate

let ( let* ) = Result.bind

(* The library's failures, in its words: the command's own are words
   already. *)
let told r = Result.map_error M.error_to_string r

let exits = function
  | Ok () -> 0
  | Error m ->
      prerr_endline m;
      1

(* One Eio loop per command, which every connection it makes lives in. *)
let in_eio f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  f ~sw ~net:(Eio.Stdenv.net env)
    ~mono_clock:(Eio.Stdenv.mono_clock env)
    ~process_mgr:(Eio.Stdenv.process_mgr env)

module Arg = Cmdliner.Arg
module Cmd = Cmdliner.Cmd
module Term = Cmdliner.Term

(* Cmdliner's [$], bound here so no term opens [Term] to reach it. *)
let ( $ ) = Term.app

let dir =
  Arg.value
    (Arg.opt Arg.string "migrations"
       (Arg.info [ "dir" ] ~docv:"DIR" ~doc:"Where the migration files are."))

(* NAME=value lines, as a .env holds them: a blank line or a # comment says
   nothing, an [export] may come before the name, and one pair of quotes
   round the value is taken off. Nothing is interpolated, and the last line
   that sets the name wins, as it would were the file sourced. *)
let from_env_file path var =
  let value line =
    let line = String.trim line in
    let export = "export " in
    let line =
      if String.starts_with ~prefix:export line then
        let n = String.length export in
        String.trim (String.sub line n (String.length line - n))
      else line
    in
    match String.index_opt line '=' with
    | Some i when String.equal (String.trim (String.sub line 0 i)) var ->
        let v =
          String.trim (String.sub line (i + 1) (String.length line - i - 1))
        in
        let n = String.length v in
        if
          n >= 2
          && (Char.equal v.[0] '"' || Char.equal v.[0] '\'')
          && Char.equal v.[n - 1] v.[0]
        then Some (String.sub v 1 (n - 2))
        else Some v
    | Some _ | None -> None
  in
  match In_channel.with_open_bin path In_channel.input_all with
  | exception Sys_error m -> Error m
  | text -> (
      match
        List.rev (List.filter_map value (String.split_on_char '\n' text))
      with
      | v :: _ when not (String.equal v "") -> Ok v
      | _ :: _ | [] -> Error (Printf.sprintf "%s does not set %s" path var))

(* The URL is [--url], or else a variable: [DATABASE_URL] unless [--env]
   names another, since the command reads no variable it was not told to
   beyond the one everybody's tools read. It is read from [--env-file] when
   one is named, and from the environment when not: what the command line
   names wins over what the shell happens to hold. *)
let url =
  let url =
    Arg.value
      (Arg.opt (Arg.some Arg.string) None
         (Arg.info [ "url" ] ~docv:"URL"
            ~doc:"The database, as a postgres:// URL or a keyword list."))
  and var =
    Arg.value
      (Arg.opt Arg.string "DATABASE_URL"
         (Arg.info [ "env" ] ~docv:"NAME"
            ~doc:
              "The variable that holds the database's URL, when $(b,--url) is \
               not given."))
  and env_file =
    Arg.value
      (Arg.opt (Arg.some Arg.string) None
         (Arg.info [ "env-file" ] ~docv:"FILE"
            ~doc:
              "Read the variable from this file of NAME=value lines, as a .env \
               holds them, rather than from the environment."))
  in
  Term.const (fun url var env_file ->
      match (url, env_file) with
      | Some u, _ -> Ok u
      | None, Some path -> from_env_file path var
      | None, None -> (
          match Sys.getenv_opt var with
          | Some u when not (String.equal (String.trim u) "") -> Ok u
          | Some _ | None ->
              Error (Printf.sprintf "no database: give --url, or set %s" var)))
  $ url $ var $ env_file

let table =
  Arg.value
    (Arg.opt Arg.string M.default_table
       (Arg.info [ "table" ] ~docv:"NAME"
          ~doc:
            "The table the database's record of its migrations is kept in, \
             after a schema and a dot if it names one."))

let lock =
  Arg.value
    (Arg.opt Arg.int M.default_lock
       (Arg.info [ "lock" ] ~docv:"N"
          ~doc:
            "The advisory lock migrating takes, the same for every process \
             that migrates one database."))

let with_db url f =
  let* url = url in
  told
    (in_eio (fun ~sw ~net ~mono_clock ~process_mgr:_ ->
         M.with_connection ~sw ~net ~mono_clock url f))

let up =
  Cmd.v
    (Cmd.info "up"
       ~doc:"Apply every migration in $(b,--dir) the database has not recorded.")
    (Term.const (fun dir url lock table ->
         exits
           (let* migrations = told (M.of_directory dir) in
            with_db url (fun conn -> M.run ~lock ~table conn migrations)))
    $ dir $ url $ lock $ table)

let status =
  let check =
    Arg.value
      (Arg.flag
         (Arg.info [ "check" ]
            ~doc:
              "Exit 1 while any migration is pending, so a script can wait on \
               it."))
  in
  let show ~check (status : M.status) =
    let number (v : M.version) = (v :> int) in
    List.iter
      (fun (v, n) -> Printf.printf "applied  %d_%s\n" (number v) n)
      status.applied;
    List.iter
      (fun (m : M.migration) ->
        Printf.printf "pending  %d_%s\n" (number m.version) m.name)
      status.pending;
    List.iter
      (fun v ->
        Printf.printf "unknown  %d (from a newer checkout)\n" (number v))
      status.unknown;
    List.iter
      (fun v -> Printf.printf "edited   %d (changed since it ran)\n" (number v))
      status.edited;
    List.iter
      (fun v ->
        Printf.printf "late     %d (older than one applied)\n" (number v))
      status.late;
    Option.iter
      (fun v ->
        Printf.printf "behind   %d (a baseline it has not reached)\n" (number v))
      status.behind;
    match (status.behind, status.unknown, status.edited, status.late) with
    | Some _, _, _, _ ->
        Error
          "the database is behind the squash: migrate it with a build from \
           before it first"
    | None, _ :: _, _, _ ->
        Error "the database was migrated from a newer checkout"
    | None, [], _ :: _, _ -> Error "a migration was edited after it ran"
    | None, [], [], _ :: _ ->
        Error "a migration is older than one already applied"
    | None, [], [], [] -> (
        match status.pending with
        | _ :: _ as pending when check ->
            Error (Printf.sprintf "%d pending" (List.length pending))
        | _ :: _ | [] -> Ok ())
  in
  Cmd.v
    (Cmd.info "status" ~doc:"List the migrations applied, and those pending.")
    (Term.const (fun dir url table check ->
         exits
           (let* migrations = told (M.of_directory dir) in
            let* status =
              with_db url (fun conn -> M.status ~table conn migrations)
            in
            show ~check status))
    $ dir $ url $ table $ check)

let new_ =
  let name =
    Arg.required
      (Arg.pos 0 (Arg.some Arg.string) None (Arg.info [] ~docv:"NAME"))
  in
  Cmd.v
    (Cmd.info "new" ~doc:"Write an empty migration, stamped with the time now.")
    (Term.const (fun dir n ->
         exits
           (Result.map print_endline
              (told (M.new_migration ~dir ~now:(Ptime_clock.now ()) n))))
    $ dir $ name)

let pg_dump =
  Arg.value
    (Arg.opt Arg.string "pg_dump --dbname={url}"
       (Arg.info [ "pg-dump" ] ~docv:"CMD"
          ~doc:
            "The pg_dump to run, which must be the server's major version. \
             $(b,{url}) and $(b,{database}) stand for the database it dumps."))

let restrict_key =
  Arg.value
    (Arg.opt Arg.string "rowtype"
       (Arg.info [ "restrict-key" ] ~docv:"KEY"
          ~doc:
            "The key pg_dump's restrict lines carry, fixed so the file is the \
             same on every run."))

let dump =
  let output =
    Arg.value
      (Arg.opt Arg.string "db/schema.sql"
         (Arg.info [ "o"; "output" ] ~docv:"FILE"
            ~doc:"Where the schema is written."))
  and check =
    Arg.value
      (Arg.flag
         (Arg.info [ "check" ]
            ~doc:
              "Write nothing; fail if $(b,--output) is not what the migrations \
               make."))
  in
  let run dir url lock table output check pg_dump restrict_key =
    let* migrations = told (M.of_directory dir) in
    let* url = url in
    let* schema =
      told
        (in_eio (fun ~sw ~net ~mono_clock ~process_mgr ->
             M.dump ~sw ~net ~mono_clock ~process_mgr ~lock ~table ~pg_dump
               ~restrict_key ~migrations url))
    in
    if check then
      match In_channel.with_open_bin output In_channel.input_all with
      | committed when String.equal committed schema -> Ok ()
      | _ | (exception Sys_error _) ->
          Error
            (Printf.sprintf
               "%s is not what the migrations make: run `rowtype-migrate dump` \
                and commit it"
               output)
    else
      match
        Out_channel.with_open_bin output (fun oc ->
            Out_channel.output_string oc schema)
      with
      | () ->
          print_endline output;
          Ok ()
      | exception Sys_error m -> Error m
  in
  Cmd.v
    (Cmd.info "dump"
       ~doc:
         "Write what the migrations add up to, dumped from a scratch database \
          on the server $(b,--url) names.")
    (Term.const (fun dir url lock table output check pg_dump key ->
         exits (run dir url lock table output check pg_dump key))
    $ dir $ url $ lock $ table $ output $ check $ pg_dump $ restrict_key)

(* The baseline is written before a file is removed, and removed only once
   the library has proved it: an interrupted squash leaves a directory
   holding both, which [of_files] refuses as two migrations at one version,
   and git holds the rest. *)
let squash =
  let through =
    Arg.required
      (Arg.pos 0 (Arg.some Arg.int) None (Arg.info [] ~docv:"VERSION"))
  in
  let run dir url lock table pg_dump restrict_key through =
    let* through =
      Option.to_result
        ~none:
          (Printf.sprintf "%d is not a migration's version: fourteen digits"
             through)
        (M.version_of_int through)
    in
    let* migrations = told (M.of_directory dir) in
    let* url = url in
    let* baseline =
      told
        (in_eio (fun ~sw ~net ~mono_clock ~process_mgr ->
             M.squash ~sw ~net ~mono_clock ~process_mgr ~lock ~table ~pg_dump
               ~restrict_key ~through ~migrations url))
    in
    let path (m : M.migration) =
      Filename.concat dir (Printf.sprintf "%d_%s.sql" (m.version :> int) m.name)
    in
    let written = path baseline in
    let replaced =
      List.filter
        (fun (m : M.migration) ->
          (m.version :> int) <= (through :> int)
          && not (String.equal (path m) written))
        migrations
    in
    let* () =
      match
        Out_channel.with_open_bin written (fun oc ->
            Out_channel.output_string oc baseline.sql)
      with
      | () -> Ok ()
      | exception Sys_error m -> Error m
    in
    let* () =
      List.fold_left
        (fun acc m ->
          let* () = acc in
          match Sys.remove (path m) with
          | () -> Ok ()
          | exception Sys_error e -> Error e)
        (Ok ()) replaced
    in
    Printf.printf "%s, in place of %d migration%s\n" written
      (List.length replaced)
      (if List.length replaced = 1 then "" else "s");
    Ok ()
  in
  Cmd.v
    (Cmd.info "squash"
       ~doc:
         "Replace every migration through $(i,VERSION) with one baseline, \
          proved on scratch databases on the server $(b,--url) names to make \
          what they made.")
    (Term.const (fun dir url lock table pg_dump key through ->
         exits (run dir url lock table pg_dump key through))
    $ dir $ url $ lock $ table $ pg_dump $ restrict_key $ through)

let on_database name ~doc f =
  Cmd.v (Cmd.info name ~doc)
    (Term.const (fun url ->
         exits
           (let* url = url in
            told
              (in_eio (fun ~sw ~net ~mono_clock ~process_mgr:_ ->
                   f ~sw ~net ~mono_clock url))))
    $ url)

let create =
  on_database "create" ~doc:"Make the database the URL names." M.create

let drop =
  on_database "drop"
    ~doc:"Drop the database the URL names, which nobody may be connected to."
    M.drop

let () =
  exit
    (Cmd.eval'
       (Cmd.group
          (Cmd.info "rowtype-migrate"
             ~doc:"A database's migrations: plain SQL files, forward only.")
          [ up; status; new_; dump; squash; create; drop ]))

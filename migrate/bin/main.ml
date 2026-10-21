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
   names wins over what the shell happens to hold. It is read here, once,
   and what the library is handed is the target it names. *)
let target =
  let url =
    Arg.value
      (Arg.opt (Arg.some Arg.string) None
         (Arg.info [ "url" ] ~docv:"URL"
            ~doc:
              "The database: a postgres:// URL or a keyword list, or \
               sqlite:PATH."))
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
      let* url =
        match (url, env_file) with
        | Some u, _ -> Ok u
        | None, Some path -> from_env_file path var
        | None, None -> (
            match Sys.getenv_opt var with
            | Some u when not (String.equal (String.trim u) "") -> Ok u
            | Some _ | None ->
                Error (Printf.sprintf "no database: give --url, or set %s" var))
      in
      (* The dialects the command was built with, SQLite's by its URL's
         [sqlite:] and Postgres's by anything else. *)
      match On_sqlite.database url with
      | Some database -> database
      | None ->
          Option.value (On_postgres.database url)
            ~default:(Error "no dialect reads this URL"))
  $ url $ var $ env_file

(* Where a package installs its migrations: the switch's [share], the
   switch being [OPAM_SWITCH_PREFIX] where opam has set it, and else the one
   this command is installed in -- its [bin]'s parent, as an image that
   copies the switch's [bin] and [share] keeps them. *)
let prefix () =
  match Sys.getenv_opt "OPAM_SWITCH_PREFIX" with
  | Some p when not (String.equal (String.trim p) "") -> p
  | Some _ | None -> Filename.dirname (Filename.dirname Sys.executable_name)

let sources =
  Arg.value
    (Arg.opt_all Arg.string []
       (Arg.info [ "source" ] ~docv:"NAME[=DIR]"
          ~doc:
            "A library's migrations, applied before the application's as a \
             history of their own: the folder the package $(i,NAME) installs, \
             $(i,share/NAME/migrations) in the switch, or $(i,DIR). Repeat it \
             for each library, in the order they run."))

(* Each [--source], read: a folder named, or the one its package installed. *)
let libraries specs =
  List.fold_left
    (fun acc spec ->
      let* acc = acc in
      let* name, migrations =
        match String.index_opt spec '=' with
        | Some i ->
            let name = String.sub spec 0 i
            and dir = String.sub spec (i + 1) (String.length spec - i - 1) in
            Result.map (fun m -> (name, m)) (told (M.of_directory dir))
        | None ->
            Result.map
              (fun m -> (spec, m))
              (told (M.of_package ~prefix:(prefix ()) spec))
      in
      let* library = told (M.source name migrations) in
      Ok (library :: acc))
    (Ok []) specs
  |> Result.map List.rev

let table =
  Arg.value
    (Arg.opt Arg.string M.default_table
       (Arg.info [ "table" ] ~docv:"NAME"
          ~doc:
            "The table the database's record of its migrations is kept in, \
             after a schema and a dot if it names one."))

let lock =
  Arg.value
    (Arg.opt (Arg.some Arg.int) None
       (Arg.info [ "lock" ] ~docv:"N"
          ~doc:
            "On Postgres, the advisory lock migrating takes, the same for \
             every process that migrates one database; the library's own \
             unless given. A SQLite file has none, and refuses it."))

let up =
  Cmd.v
    (Cmd.info "up"
       ~doc:"Apply every migration in $(b,--dir) the database has not recorded.")
    (Term.const (fun dir target lock table sources ->
         exits
           (let* migrations = told (M.of_directory dir) in
            let* libraries = libraries sources in
            let* (db : Database.t) = target in
            db.apply ~lock ~table ~libraries migrations))
    $ dir $ target $ lock $ table $ sources)

let status =
  let check =
    Arg.value
      (Arg.flag
         (Arg.info [ "check" ]
            ~doc:
              "Exit 1 while any migration is pending, so a script can wait on \
               it."))
  in
  (* One source's lines; with libraries, each line names its source first. *)
  let lines ~named (status : M.status) =
    let number (v : M.version) = (v :> int) in
    let line fmt =
      if named then Printf.printf ("%-12s " ^^ fmt) status.source
      else Printf.printf fmt
    in
    List.iter
      (fun (v, n) -> line "applied  %d_%s\n" (number v) n)
      status.applied;
    List.iter
      (fun (m : M.migration) ->
        line "pending  %d_%s\n" (number m.version) m.name)
      status.pending;
    List.iter
      (fun v -> line "unknown  %d (from a newer checkout)\n" (number v))
      status.unknown;
    List.iter
      (fun (m : M.migration) ->
        line "edited   %d_%s (changed since it ran)\n" (number m.version) m.name)
      status.edited;
    List.iter
      (fun (m : M.migration) ->
        line "late     %d_%s (older than one applied)\n" (number m.version)
          m.name)
      status.late;
    Option.iter
      (fun (m : M.migration) ->
        line "behind   %d_%s (a baseline it has not reached)\n"
          (number m.version) m.name)
      status.behind
  in
  let show ~check (statuses : M.status list) =
    let named = List.length statuses > 1 in
    List.iter (lines ~named) statuses;
    match List.find_map M.refusal statuses with
    | Some e -> Error (M.error_to_string e)
    | None -> (
        match List.concat_map (fun (s : M.status) -> s.pending) statuses with
        | _ :: _ as pending when check ->
            Error (Printf.sprintf "%d pending" (List.length pending))
        | _ :: _ | [] -> Ok ())
  in

  Cmd.v
    (Cmd.info "status" ~doc:"List the migrations applied, and those pending.")
    (Term.const (fun dir target table check sources ->
         exits
           (let* migrations = told (M.of_directory dir) in
            let* libraries = libraries sources in
            let* (db : Database.t) = target in
            let* statuses = db.status ~table ~libraries migrations in
            show ~check statuses))
    $ dir $ target $ table $ check $ sources)

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
    (Arg.opt (Arg.some Arg.string) None
       (Arg.info [ "pg-dump" ] ~docv:"CMD"
          ~doc:
            "On Postgres, the pg_dump to run, which must be the server's major \
             version, $(b,pg_dump --dbname={url}) unless given. $(b,{url}) and \
             $(b,{database}) stand for the database it dumps."))

let restrict_key =
  Arg.value
    (Arg.opt (Arg.some Arg.string) None
       (Arg.info [ "restrict-key" ] ~docv:"KEY"
          ~doc:
            "On Postgres, the key pg_dump's restrict lines carry, fixed so the \
             file is the same on every run, $(b,rowtype) unless given."))

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
  let run dir target table output check pg_dump restrict_key sources =
    let* migrations = told (M.of_directory dir) in
    let* libraries = libraries sources in
    let* (db : Database.t) = target in
    let* schema = db.dump ~table ~libraries ~pg_dump ~restrict_key migrations in
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
         "Write what the migrations add up to, dumped from a scratch database: \
          on Postgres, one made on the server $(b,--url) names and dropped \
          after; on SQLite, one in memory, nothing written beside the file.")
    (Term.const (fun dir target table output check pg_dump key sources ->
         exits (run dir target table output check pg_dump key sources))
    $ dir $ target $ table $ output $ check $ pg_dump $ restrict_key $ sources)

(* The baseline is written before a file is removed, and removed only once
   the library has proved it: an interrupted squash leaves a directory
   holding both, which [of_files] refuses as two migrations at one version,
   and git holds the rest. *)
let squash =
  let through =
    Arg.required
      (Arg.pos 0 (Arg.some Arg.int) None (Arg.info [] ~docv:"VERSION"))
  in
  let run dir target table pg_dump restrict_key through sources =
    let* through =
      Option.to_result
        ~none:
          (Printf.sprintf "%d is not a migration's version: fourteen digits"
             through)
        (M.version_of_int through)
    in
    let* migrations = told (M.of_directory dir) in
    let* libraries = libraries sources in
    let* (db : Database.t) = target in
    let* baseline =
      db.squash ~table ~libraries ~pg_dump ~restrict_key ~through migrations
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
          proved to make what they made on scratch databases: on Postgres, \
          made on the server $(b,--url) names and dropped after; on SQLite, in \
          memory.")
    (Term.const (fun dir target table pg_dump key through sources ->
         exits (run dir target table pg_dump key through sources))
    $ dir $ target $ table $ pg_dump $ restrict_key $ through $ sources)

let on_database name ~doc f =
  Cmd.v (Cmd.info name ~doc)
    (Term.const (fun target ->
         exits
           (let* (db : Database.t) = target in
            f db))
    $ target)

let create =
  on_database "create" ~doc:"Make the database the URL names." (fun db ->
      db.create ())

let drop =
  on_database "drop"
    ~doc:
      "Drop the database the URL names; on Postgres, one nobody is connected \
       to." (fun db -> db.drop ())

(* What the libraries could not answer as a value -- a scratch database
   left on the server -- reaches the terminal as a warning or worse, and
   nothing quieter does. *)
let () =
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level (Some Logs.Warning);
  exit
    (Cmd.eval'
       (Cmd.group
          (Cmd.info "rowtype-migrate"
             ~doc:"A database's migrations: plain SQL files, forward only.")
          [ up; status; new_; dump; squash; create; drop ]))

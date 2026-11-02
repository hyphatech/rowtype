module Pg = Rowtype_postgres

type runtime = [ `Podman | `Docker ]

type error =
  [ `Runtime of runtime * string | `Not_ready of string | `Unreadable of string ]

let error_to_string = function
  | `Runtime (`Podman, why) -> "Podman: " ^ why
  | `Runtime (`Docker, why) -> "Docker: " ^ why
  | `Not_ready why -> "Postgres did not answer: " ^ why
  | `Unreadable why -> "not a connection string: " ^ why
  | #Rowtype.error as e -> Rowtype.error_to_string e

(* What a cleanup could not remove, which has no caller left to answer. *)
let src =
  Logs.Src.create "rowtype-test" ~doc:"What a run could not remove after it"

module Log = (val Logs.src_log src : Logs.LOG)

type server = {
  conninfo : Pg.Conninfo.t;
  container : (runtime * string) option;
  connect : sw:Eio.Switch.t -> Pg.Conninfo.t -> (Pg.conn, Rowtype.error) result;
  sw : Eio.Switch.t;
}

let conninfo s = s.conninfo
let container s = s.container

(* Every container this library starts carries the process that started it,
   so one a killed run left is told from one a live run holds. *)
let label = "rowtype.test.pid"

(* A test keeps nothing, so the data is in memory and nothing is synced. *)
let default_image = "docker.io/library/postgres:18-alpine"

(* Long enough for an image's first start, which runs its own setup, on a
   loaded machine; a server that answers sooner is used at once. *)
let ready_within = Mtime.Span.(60 * s)
let between_tries = Mtime.Span.(100 * ms)

(* Podman's and Docker's command lines take the same arguments for all
   this library asks of them, so the runtime is its binary alone. *)
let binary = function `Podman -> "podman" | `Docker -> "docker"
let container_to_string (runtime, name) = binary runtime ^ " " ^ name

let container_of_string words =
  match String.split_on_char ' ' words with
  | [ "podman"; name ] when not (String.equal name "") -> Some (`Podman, name)
  | [ "docker"; name ] when not (String.equal name "") -> Some (`Docker, name)
  | _ -> None

let cli runtime mgr args =
  let out = Buffer.create 256 and err = Buffer.create 256 in
  match
    Eio.Process.run mgr ~stdout:(Eio.Flow.buffer_sink out)
      ~stderr:(Eio.Flow.buffer_sink err) (binary runtime :: args)
  with
  | () -> Ok (String.trim (Buffer.contents out))
  | exception Eio.Io (Eio.Process.E (Child_error _), _) ->
      Error
        (`Runtime
           (runtime, String.trim (Buffer.contents err ^ Buffer.contents out)))
  | exception Eio.Io (Eio.Process.E (Executable_not_found _), _) ->
      Error
        (`Runtime
           ( runtime,
             Printf.sprintf "%s is not on PATH: install it, or use %s"
               (binary runtime)
               (match runtime with `Podman -> "Docker" | `Docker -> "Podman") ))
  | exception (Eio.Io _ as ex) ->
      Error
        (`Runtime
           ( runtime,
             Format.asprintf "%s could not be run: %a" (binary runtime)
               Eio.Exn.pp ex ))

let alive pid =
  match Unix.kill pid 0 with
  | () -> true
  | exception Unix.Unix_error (Unix.ESRCH, _, _) -> false
  | exception Unix.Unix_error _ -> true

(* The next run is the reaper: what a run that ended left, it removes. A
   label is read by [inspect], whose template is the same in every Podman;
   [ps]'s changed between versions. *)
let remove_left runtime mgr =
  match cli runtime mgr [ "ps"; "-a"; "-q"; "--filter"; "label=" ^ label ] with
  | Error _ -> ()
  | Ok listing ->
      List.iter
        (fun id ->
          match
            cli runtime mgr
              [
                "inspect";
                "--format";
                Printf.sprintf "{{index .Config.Labels %S}}" label;
                id;
              ]
          with
          | Ok pid -> (
              match int_of_string_opt pid with
              | Some pid when not (alive pid) -> (
                  match cli runtime mgr [ "rm"; "-f"; "-v"; id ] with
                  | Ok _ -> ()
                  | Error e ->
                      Log.warn (fun m ->
                          m "a container a killed run left, %s, stays: %s" id
                            (error_to_string e)))
              | Some _ | None -> ())
          | Error _ -> ())
        (List.filter
           (fun l -> not (String.equal l ""))
           (String.split_on_char '\n' listing))

let port_of listing =
  (* [port] answers "127.0.0.1:41234", or one line a family. *)
  match String.split_on_char '\n' listing with
  | first :: _ -> (
      match String.rindex_opt first ':' with
      | Some i ->
          int_of_string_opt
            (String.sub first (i + 1) (String.length first - i - 1))
      | None -> None)
  | [] -> None

let rec until_ready ~clock ~deadline connect conninfo =
  match
    Eio.Switch.run (fun sw -> Result.map Pg.close (connect ~sw conninfo))
  with
  | Ok () -> Ok ()
  | Error e ->
      if Mtime.is_later (Eio.Time.Mono.now clock) ~than:deadline then
        Error (`Not_ready (Rowtype.error_to_string e))
      else (
        Eio.Time.Mono.sleep_span clock between_tries;
        until_ready ~clock ~deadline connect conninfo)

let started ~sw ~runtime ~image ~clock mgr connect =
  let ( let* ) = Result.bind in
  remove_left runtime mgr;
  let name =
    Printf.sprintf "rowtype-test-%d-%d" (Unix.getpid ())
      (Random.State.bits (Random.State.make_self_init ()))
  in
  let* _ =
    cli runtime mgr
      [
        "run";
        "-d";
        "--rm";
        "--name";
        name;
        "--label";
        Printf.sprintf "%s=%d" label (Unix.getpid ());
        "-e";
        "POSTGRES_USER=test";
        "-e";
        "POSTGRES_PASSWORD=test";
        "-e";
        "POSTGRES_DB=test";
        "-p";
        "127.0.0.1::5432";
        "--tmpfs";
        "/var/lib/postgresql";
        image;
        "postgres";
        "-c";
        "fsync=off";
        "-c";
        "synchronous_commit=off";
        "-c";
        "full_page_writes=off";
      ]
  in
  (* Removed as the run's switch ends, however it ends. *)
  (* Protected, since a switch that failed is cancelled as it ends, when
     this matters most. *)
  Eio.Switch.on_release sw (fun () ->
      Eio.Cancel.protect (fun () ->
          match cli runtime mgr [ "rm"; "-f"; "-v"; name ] with
          | Ok _ -> ()
          | Error e ->
              Log.warn (fun m ->
                  m "the run's container, %s, stays: %s" name
                    (error_to_string e))));
  let* listing = cli runtime mgr [ "port"; name; "5432" ] in
  let* port =
    Option.to_result
      ~none:(`Runtime (runtime, "no port in " ^ listing))
      (port_of listing)
  in
  let* conninfo =
    Result.map_error
      (fun m -> `Unreadable m)
      (Pg.Conninfo.of_string
         (Printf.sprintf "postgres://test:test@127.0.0.1:%d/test" port))
  in
  let deadline =
    Option.value ~default:(Eio.Time.Mono.now clock)
      (Mtime.add_span (Eio.Time.Mono.now clock) ready_within)
  in
  let* () = until_ready ~clock ~deadline connect conninfo in
  Ok { conninfo; container = Some (runtime, name); connect; sw }

let server ~sw ?url ?container ?(runtime = `Podman) ?(image = default_image) env
    =
  let mgr = Eio.Stdenv.process_mgr env and clock = Eio.Stdenv.mono_clock env in
  (* Each connection on a switch of its own, opened and closed by its use:
     one made as the run's switch ends, to drop what it made, must not be
     the run's. *)
  let connect ~sw conninfo =
    Pg.connect ~sw ~net:(Eio.Stdenv.net env) ~mono_clock:clock conninfo
  in
  match url with
  | Some url -> (
      match Pg.Conninfo.of_string url with
      | Ok conninfo -> Ok { conninfo; container; connect; sw }
      | Error m -> Error (`Unreadable m))
  | None ->
      (started ~sw ~runtime ~image ~clock mgr connect
        :> (server, [> error ]) result)

(* A database per asking, templates and copies alike: the process and a
   counter name it, since the suites of one run share a server and run at
   once. *)
let asked = Atomic.make 0

let named kind =
  Printf.sprintf "rowtype_%s_%d_%d" kind (Unix.getpid ())
    (Atomic.fetch_and_add asked 1)

let admin s sql =
  Eio.Switch.run @@ fun sw ->
  let ( let* ) = Result.bind in
  let* db = s.connect ~sw s.conninfo in
  Fun.protect ~finally:(fun () -> Pg.close db) (fun () -> Pg.exec_raw db sql)

type template = { server : server; name : string }

let widen r = (r : (_, Rowtype.error) result :> (_, [> Rowtype.error ]) result)

let template ?(setup = fun _ -> Ok ()) server =
  let ( let* ) = Result.bind in
  let name = named "template" in
  let* () = widen (admin server ("create database " ^ name)) in
  (* Dropped with the server's switch, a template no longer first. *)
  (* Two statements, since DROP DATABASE refuses to run in the transaction
     one query of two would be; protected, as the container's removal is. *)
  Eio.Switch.on_release server.sw (fun () ->
      Eio.Cancel.protect (fun () ->
          match
            Result.bind
              (admin server
                 (Printf.sprintf "alter database %s is_template false" name))
              (fun () ->
                admin server
                  (Printf.sprintf "drop database if exists %s with (force)" name))
          with
          | Ok () -> ()
          | Error e ->
              Log.warn (fun m ->
                  m "the template %s stays: %s" name (Rowtype.error_to_string e))));
  let* () = setup (Pg.Conninfo.with_database server.conninfo name) in
  (* A copy is refused while anyone is connected to its template, so the
     setup's connections end here, and none may open after. *)
  let* () =
    widen
      (admin server
         (Printf.sprintf
            "alter database %s with is_template true allow_connections false; \
             select pg_terminate_backend(pid) from pg_stat_activity where \
             datname = '%s' and pid <> pg_backend_pid()"
            name name))
  in
  Ok { server; name }

let database t f =
  let ( let* ) = Result.bind in
  let name = named "test" in
  let* () =
    widen
      (admin t.server
         (Printf.sprintf "create database %s template %s" name t.name))
  in
  (* Dropped with (force), so a connection the case left open does not keep
     it, and whatever the case did; protected, since a cancelled case may not
     connect to drop it. *)
  let drop () =
    Eio.Cancel.protect (fun () ->
        admin t.server
          (Printf.sprintf "drop database if exists %s with (force)" name))
  in
  (* A copy left is the case's failure where it answered; where it raised,
     the raise is what the caller hears, and the copy is a warning. *)
  match f (Pg.Conninfo.with_database t.server.conninfo name) with
  | exception ex ->
      let trace = Printexc.get_raw_backtrace () in
      (match drop () with
      | Ok () -> ()
      | Error e ->
          Log.warn (fun m ->
              m "a case's database, %s, stays: %s" name
                (Rowtype.error_to_string e)));
      Printexc.raise_with_backtrace ex trace
  | answer -> widen (Result.map (fun () -> answer) (drop ()))

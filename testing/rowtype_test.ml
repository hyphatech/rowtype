module Pg = Rowtype_postgres

type error =
  [ `Runtime of string | `Not_ready of string | `Unreadable of string ]

let error_to_string = function
  | `Runtime why -> "Podman: " ^ why
  | `Not_ready why -> "Postgres did not answer: " ^ why
  | `Unreadable why -> "not a connection string: " ^ why

type server = {
  conninfo : Pg.Conninfo.t;
  container : string option;
  connect : Pg.Conninfo.t -> (Pg.conn, Rowtype.error) result;
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

let podman mgr args =
  let out = Buffer.create 256 and err = Buffer.create 256 in
  match
    Eio.Process.run mgr ~stdout:(Eio.Flow.buffer_sink out)
      ~stderr:(Eio.Flow.buffer_sink err) ("podman" :: args)
  with
  | () -> Ok (String.trim (Buffer.contents out))
  | exception Eio.Io _ ->
      Error (`Runtime (String.trim (Buffer.contents err ^ Buffer.contents out)))

let alive pid =
  match Unix.kill pid 0 with
  | () -> true
  | exception Unix.Unix_error (Unix.ESRCH, _, _) -> false
  | exception Unix.Unix_error _ -> true

(* The next run is the reaper: what a run that ended left, it removes. *)
let remove_left mgr =
  match
    podman mgr
      [
        "ps";
        "-a";
        "--filter";
        "label=" ^ label;
        "--format";
        Printf.sprintf "{{.Names}} {{.Label %S}}" label;
      ]
  with
  | Error _ -> ()
  | Ok listing ->
      List.iter
        (fun line ->
          match String.split_on_char ' ' line with
          | [ name; pid ] -> (
              match int_of_string_opt pid with
              | Some pid when not (alive pid) ->
                  ignore
                    (podman mgr [ "rm"; "-f"; "-v"; name ]
                      : (string, [> error ]) result)
              | Some _ | None -> ())
          | _ -> ())
        (String.split_on_char '\n' listing)

let port_of listing =
  (* [podman port] answers "127.0.0.1:41234", or one line a family. *)
  match String.split_on_char '\n' listing with
  | first :: _ -> (
      match String.rindex_opt first ':' with
      | Some i ->
          int_of_string_opt
            (String.sub first (i + 1) (String.length first - i - 1))
      | None -> None)
  | [] -> None

let rec until_ready ~clock ~deadline connect conninfo =
  match connect conninfo with
  | Ok db ->
      Pg.close db;
      Ok ()
  | Error e ->
      if Mtime.is_later (Eio.Time.Mono.now clock) ~than:deadline then
        Error (`Not_ready (Rowtype.error_to_string e))
      else (
        Eio.Time.Mono.sleep_span clock between_tries;
        until_ready ~clock ~deadline connect conninfo)

let started ~sw ~image ~clock mgr connect =
  let ( let* ) = Result.bind in
  remove_left mgr;
  let name =
    Printf.sprintf "rowtype-test-%d-%d" (Unix.getpid ())
      (Random.State.bits (Random.State.make_self_init ()))
  in
  let* _ =
    podman mgr
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
  Eio.Switch.on_release sw (fun () ->
      ignore
        (podman mgr [ "rm"; "-f"; "-v"; name ] : (string, [> error ]) result));
  let* listing = podman mgr [ "port"; name; "5432" ] in
  let* port =
    Option.to_result
      ~none:(`Runtime ("no port in " ^ listing))
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
  Ok { conninfo; container = Some name; connect }

let server ~sw ?url ?(image = default_image) env =
  let mgr = Eio.Stdenv.process_mgr env and clock = Eio.Stdenv.mono_clock env in
  let connect conninfo =
    Pg.connect ~sw ~net:(Eio.Stdenv.net env) ~mono_clock:clock conninfo
  in
  match url with
  | Some url -> (
      match Pg.Conninfo.of_string url with
      | Ok conninfo -> Ok { conninfo; container = None; connect }
      | Error m -> Error (`Unreadable m))
  | None ->
      (started ~sw ~image ~clock mgr connect :> (server, [> error ]) result)

(* One database per asking: the process and a counter name it, since the
   suites of one run share a server and run at once. *)
let asked = Atomic.make 0

let database s f =
  let ( let* ) = Result.bind in
  let widen r =
    (r : (_, Rowtype.error) result :> (_, [> Rowtype.error ]) result)
  in
  let name =
    Printf.sprintf "rowtype_test_%d_%d" (Unix.getpid ())
      (Atomic.fetch_and_add asked 1)
  in
  let admin sql =
    let* db = s.connect s.conninfo in
    Fun.protect ~finally:(fun () -> Pg.close db) (fun () -> Pg.exec_raw db sql)
  in
  let* () = widen (admin ("create database " ^ name)) in
  (* Dropped with (force), so a connection the case left open does not keep
     it, and whatever the case did. *)
  let drop () =
    ignore
      (admin (Printf.sprintf "drop database if exists %s with (force)" name)
        : (unit, Rowtype.error) result)
  in
  Ok
    (Fun.protect ~finally:drop (fun () ->
         f (Pg.Conninfo.with_database s.conninfo name)))

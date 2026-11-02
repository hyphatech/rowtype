(* Where a test's database comes from.

   Every suite that touches a database needs a Postgres server: the one
   TEST_DATABASE_URL names, where a runner hands one, or else one of the
   suite's own, which rowtype-test starts in Podman and removes as the suite
   ends. The suite reads the variable where it starts, as an executable
   does; this is what it reads it for. *)

let variable = "TEST_DATABASE_URL"

(* The Eio loop a suite runs its cases in, a switch that lasts as long and
   the server on it: a connection is Eio's, and a suite's cases are
   functions of nothing, so the one [Eio_main.run] a suite starts in hands
   them over once, with [start]. *)
let loop :
    (Eio_unix.Stdenv.base
    * Eio.Switch.t
    * Rowtype_test.server
    * Rowtype_test.template)
    option
    ref =
  ref None

(* The suite's template is empty: each case makes its own tables. *)
let start env ~sw ?url () =
  match
    Result.bind (Rowtype_test.server ~sw ?url env) (fun server ->
        Result.map (fun t -> (server, t)) (Rowtype_test.template server))
  with
  | Ok (server, template) ->
      loop := Some ((env :> Eio_unix.Stdenv.base), sw, server, template)
  | Error e ->
      prerr_endline ("no test database: " ^ Rowtype_test.error_to_string e);
      exit 1

let io () =
  match !loop with
  | Some v -> v
  | None -> Alcotest.fail "the suite did not call Db_target.start"

let sw () =
  let _, sw, _, _ = io () in
  sw

let env () =
  let env, _, _, _ = io () in
  env

let net () = Eio.Stdenv.net (env ())
let mono () = Eio.Stdenv.mono_clock (env ())

let server () =
  let _, _, server, _ = io () in
  Rowtype_postgres.Conninfo.to_url (Rowtype_test.conninfo server)

let pg_dump () =
  let _, _, server, _ = io () in
  Option.map
    (fun (runtime, name) ->
      let binary =
        match runtime with `Podman -> "podman" | `Docker -> "docker"
      in
      Printf.sprintf "%s exec %s pg_dump -U test -d {database}" binary name)
    (Rowtype_test.container server)

let conninfo url =
  match Rowtype_postgres.Conninfo.of_string url with
  | Ok c -> c
  | Error m -> Alcotest.failf "%s: %s" variable m

(* The server the suite was given, naming another database. *)
let on_database name =
  Rowtype_postgres.Conninfo.with_database (conninfo (server ())) name

(* [statement_cache:0] is a connection that keeps no statements, whose
   results all arrive as text: the other way every cell is read. *)
let connect ?statement_cache target =
  match
    Rowtype_postgres.connect ~sw:(sw ()) ~net:(net ()) ~mono_clock:(mono ())
      ?statement_cache target
  with
  | Ok db -> db
  | Error e -> Alcotest.failf "cannot connect: %s" (Rowtype.error_to_string e)

let admin f =
  let db = connect (conninfo (server ())) in
  Fun.protect ~finally:(fun () -> Rowtype_postgres.close db) (fun () -> f db)

let exec db sql =
  match Rowtype_postgres.exec_raw db sql with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%s: %s" sql (Rowtype.error_to_string e)

(* A database of its own per asking, so one case cannot see another's rows
   and a failure leaves nothing behind for the next run to trip over. *)
let with_postgres f =
  let _, _, _, template = io () in
  match Rowtype_test.database template f with
  | Ok v -> v
  | Error e ->
      Alcotest.failf "a database of its own: %s" (Rowtype.error_to_string e)

(* The cases write an instant as epoch microseconds, a figure checked
   against Postgres's own; this is a shape of instants in that unit,
   converted through whole seconds and their fraction, apart from how the
   backend does it. *)
let in_us ty =
  let us_per_s = 1_000_000 and ps_per_us = 1_000_000L in
  Rowtype.map ty
    ~parse:(fun t ->
      let whole = Ptime.truncate ~frac_s:0 t in
      match Ptime.Span.to_int_s (Ptime.to_span whole) with
      | None -> Alcotest.fail "an instant past an int of seconds"
      | Some s ->
          let _, ps = Ptime.Span.to_d_ps (Ptime.frac_s t) in
          (s * us_per_s) + Int64.to_int (Int64.div ps ps_per_us))
    ~print:(fun us ->
      let s = Int.div us us_per_s - if us mod us_per_s < 0 then 1 else 0 in
      let frac = us - (s * us_per_s) in
      match
        Option.bind
          (Ptime.Span.of_d_ps (0, Int64.mul (Int64.of_int frac) ps_per_us))
          (fun f -> Ptime.of_span (Ptime.Span.add (Ptime.Span.of_int_s s) f))
      with
      | Some t -> t
      | None -> Alcotest.failf "%d microseconds is no instant" us)

let instant_us = in_us Rowtype.instant
let timestamp_us = in_us Rowtype.timestamp

(* The cases write a uuid as its text; this is the uuid in that form, read
   back lowercase as Uuidm prints it. *)
let uuid_text =
  Rowtype.map Rowtype.uuid ~parse:Uuidm.to_string ~print:(fun s ->
      match Uuidm.of_string s with
      | Some u -> u
      | None -> Alcotest.failf "%S is no uuid" s)

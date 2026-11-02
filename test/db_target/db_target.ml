(* Where a test's database comes from.

   Every suite that touches a database needs a Postgres server: the one
   TEST_DATABASE_URL names, where a runner hands one, or else one of the
   suite's own, which rowtype-test starts and removes as the suite ends.
   The suite reads the variable where it starts, as an executable does,
   and hands what [start] makes to each case that needs it. *)

let variable = "TEST_DATABASE_URL"
let container_variable = "ROWTYPE_TEST_CONTAINER"

type error = [ Rowtype_test.error | Rowtype.error | `Container of string ]

let error_to_string = function
  | `Container words -> container_variable ^ " names no container: " ^ words
  | #Rowtype_test.error as e -> Rowtype_test.error_to_string e
  | #Rowtype.error as e -> Rowtype.error_to_string e

type t = {
  env : Eio_unix.Stdenv.base;
  sw : Eio.Switch.t;
  server : Rowtype_test.server;
  template : Rowtype_test.template;
}

(* The suite's template is empty: each case makes its own tables. *)
let start env ~sw ?url ?container () =
  let ( let* ) = Result.bind in
  let* container =
    match container with
    | None -> Ok None
    | Some words -> (
        match Rowtype_test.container_of_string words with
        | Some c -> Ok (Some c)
        | None -> Error (`Container words))
  in
  Result.bind (Rowtype_test.server ~sw ?url ?container env) (fun server ->
      Result.map
        (fun template -> { env :> Eio_unix.Stdenv.base; sw; server; template })
        (Rowtype_test.template server))

let env t = t.env
let sw t = t.sw
let net t = Eio.Stdenv.net t.env
let mono t = Eio.Stdenv.mono_clock t.env

let pg_dump t =
  Option.map
    (fun (runtime, name) ->
      let binary =
        match runtime with `Podman -> "podman" | `Docker -> "docker"
      in
      Printf.sprintf "%s exec %s pg_dump -U test -d {database}" binary name)
    (Rowtype_test.container t.server)

let conninfo url =
  match Rowtype_postgres.Conninfo.of_string url with
  | Ok c -> c
  | Error m -> Alcotest.failf "%s: %s" variable m

(* The server the suite was given, naming another database. *)
let on_database t name =
  Rowtype_postgres.Conninfo.with_database (Rowtype_test.conninfo t.server) name

(* [statement_cache:0] is a connection that keeps no statements, whose
   results all arrive as text: the other way every cell is read. *)
let connect t ?statement_cache target =
  match
    Rowtype_postgres.connect ~sw:t.sw ~net:(net t) ~mono_clock:(mono t)
      ?statement_cache target
  with
  | Ok db -> db
  | Error e -> Alcotest.failf "cannot connect: %s" (Rowtype.error_to_string e)

let admin t f =
  let db = connect t (Rowtype_test.conninfo t.server) in
  Fun.protect ~finally:(fun () -> Rowtype_postgres.close db) (fun () -> f db)

let exec db sql =
  match Rowtype_postgres.exec_raw db sql with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%s: %s" sql (Rowtype.error_to_string e)

(* A database of its own per asking, so one case cannot see another's rows
   and a failure leaves nothing behind for the next run to trip over. *)
let with_postgres t f =
  match Rowtype_test.database t.template f with
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

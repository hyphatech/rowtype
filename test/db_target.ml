(* Where a test's database comes from.

   Every suite that touches a database needs a Postgres server, and names it
   with ROWTYPE_TEST_PG -- which `make test` sets after bringing the tests'
   own container up. A bare `dune test` leaves it unset, and those suites
   then skip themselves and SAY so, rather than failing on a machine with
   no Docker or passing without having run. {!required} is that seam. *)

let base = Sys.getenv_opt "ROWTYPE_TEST_PG"

(* Called first by every suite that needs a database. Without one it prints
   why and exits cleanly, so a skipped suite is a line in the output and not
   a silent green. *)
let required ~suite =
  if Option.is_none base then begin
    Printf.printf
      "  [%s skipped: no ROWTYPE_TEST_PG -- set it to a Postgres server's URL]\n\
       %!"
      suite;
    exit 0
  end

(* The Eio loop a suite runs its cases in, and a switch that lasts as long:
   a connection is Eio's, and a suite's cases are functions of nothing, so
   the one [Eio_main.run] a suite starts in hands both over once, with
   [eio env ~sw]. *)
let loop : (Eio_unix.Stdenv.base * Eio.Switch.t) option ref = ref None
let eio env ~sw = loop := Some ((env :> Eio_unix.Stdenv.base), sw)

let io () =
  match !loop with
  | Some v -> v
  | None -> Alcotest.fail "the suite did not call Db_target.eio env ~sw"

let sw () = snd (io ())
let net () = Eio.Stdenv.net (fst (io ()))
let mono () = Eio.Stdenv.mono_clock (fst (io ()))

let server () =
  match base with
  | None -> Alcotest.fail "ROWTYPE_TEST_PG is not set"
  | Some server -> server

(* The server a URL names, with the database path replaced. *)
let on_database name =
  match Rowtype_postgres.on_database ~server:(server ()) name with
  | Ok url -> url
  | Error e -> Alcotest.failf "ROWTYPE_TEST_PG: %s" (Rowtype.error_to_string e)

(* [statement_cache:0] is a connection that keeps no statements, whose
   results all arrive as text: the other way every cell is read. *)
let connect ?statement_cache url =
  match
    Result.bind
      (Rowtype_postgres.conninfo url)
      (Rowtype_postgres.connect ~sw:(sw ()) ~net:(net ()) ~mono_clock:(mono ())
         ?statement_cache)
  with
  | Ok db -> db
  | Error e -> Alcotest.failf "cannot connect: %s" (Rowtype.error_to_string e)

let admin f =
  let db = connect (server ()) in
  Fun.protect ~finally:(fun () -> Rowtype_postgres.close db) (fun () -> f db)

let exec db sql =
  match Rowtype_postgres.exec_raw db sql with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%s: %s" sql (Rowtype.error_to_string e)

(* A database of its own per asking, so one case cannot see another's rows
   and a failure leaves nothing behind for the next run to trip over. The
   name carries the pid because `dune test` runs the executables in
   parallel, and a counter because one executable asks many times.

   [with (force)] because a connection the test failed to close would
   otherwise make the drop fail and leave the database behind -- and a
   leaked database is a slow leak nobody notices until the container is
   full. *)
let nth = ref 0

let with_postgres f =
  incr nth;
  let name = Printf.sprintf "rowtype_test_%d_%d" (Unix.getpid ()) !nth in
  admin (fun db -> exec db (Printf.sprintf "create database %s" name));
  let finally () =
    admin (fun db ->
        exec db (Printf.sprintf "drop database if exists %s with (force)" name))
  in
  Fun.protect ~finally (fun () -> f (on_database name))

(* The cases write an instant as epoch microseconds, a figure checked
   against Postgres's own; this is a shape of instants in that unit,
   converted through whole seconds and their fraction, apart from how the
   backend does it. *)
let in_us ty =
  let us_per_s = 1_000_000 and ps_per_us = 1_000_000L in
  Rowtype.conv ty
    ~of_:(fun t ->
      let whole = Ptime.truncate ~frac_s:0 t in
      match Ptime.Span.to_int_s (Ptime.to_span whole) with
      | None -> Alcotest.fail "an instant past an int of seconds"
      | Some s ->
          let _, ps = Ptime.Span.to_d_ps (Ptime.frac_s t) in
          (s * us_per_s) + Int64.to_int (Int64.div ps ps_per_us))
    ~to_:(fun us ->
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
  Rowtype.conv Rowtype.uuid ~of_:Uuidm.to_string ~to_:(fun s ->
      match Uuidm.of_string s with
      | Some u -> u
      | None -> Alcotest.failf "%S is no uuid" s)

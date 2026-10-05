(* The migration files' pieces, without a database: reading them, naming a
   new one, and what a squash refuses before it connects. *)

module C = Rowtype_migrate

let check_string = Alcotest.(check string)
let words = C.error_to_string
let number (v : C.version) = (v :> int)

let temp_dir () =
  let d = Filename.temp_dir "rowtype_migrate_files" "" in
  at_exit (fun () ->
      Array.iter (fun f -> Sys.remove (Filename.concat d f)) (Sys.readdir d);
      Sys.rmdir d);
  d

let file dir name text =
  let path = Filename.concat dir name in
  Out_channel.with_open_bin path (fun oc -> Out_channel.output_string oc text);
  path

(* Files are read in version order whatever order they are named in, and a
   name that is not a migration's, or a version used twice, is refused by
   name rather than skipped. *)
let test_files_are_read_in_version_order () =
  let dir = temp_dir () in
  let b = file dir "20260102000000_second.sql" "select 2;"
  and a = file dir "20260101000000_first.sql" "select 1;" in
  (match C.of_files [ b; a ] with
  | Error e -> Alcotest.fail (words e)
  | Ok ms ->
      Alcotest.(check (list (pair int string)))
        "in order"
        [ (20260101000000, "first"); (20260102000000, "second") ]
        (List.map (fun (m : C.migration) -> (number m.version, m.name)) ms));
  let refused paths =
    match C.of_files paths with Ok _ -> false | Error _ -> true
  in
  Alcotest.(check bool)
    "a name that is not a migration's" true
    (refused [ file dir "first.sql" "" ]);
  Alcotest.(check bool)
    "a short stamp" true
    (refused [ file dir "2026_short.sql" "" ]);
  Alcotest.(check bool)
    "one version twice" true
    (refused [ a; file dir "20260101000000_again.sql" "" ]);
  match C.of_directory dir with
  | Ok _ -> Alcotest.fail "a directory holding a stray file was read"
  | Error (`Files m) ->
      Alcotest.(check bool) ("names it: " ^ m) true (String.length m > 0)
  | Error e -> Alcotest.failf "not the files': %s" (words e)

(* A directive is the first line or nothing, and one not known is refused
   rather than read as a comment. *)
let test_a_directive_is_the_first_line () =
  let dir = temp_dir () in
  let kind_of name text =
    match C.of_files [ file dir name text ] with
    | Ok [ m ] -> Ok m.kind
    | Ok _ -> Error "not one migration"
    | Error e -> Error (words e)
  in
  let is kind read =
    match (kind, read) with
    | C.Transaction, Ok C.Transaction | C.No_transaction, Ok C.No_transaction ->
        true
    | _ -> false
  in
  Alcotest.(check bool)
    "outside a transaction" true
    (is C.No_transaction
       (kind_of "20260101000000_index.sql"
          "-- rowtype-migrate: no transaction\n\
           create index concurrently if not exists i on t (n);\n"));
  Alcotest.(check bool)
    "in one, unless it says" true
    (is C.Transaction
       (kind_of "20260102000000_plain.sql" "create table t (n int);\n"));
  Alcotest.(check bool)
    "a directive below the first line is a comment" true
    (is C.Transaction
       (kind_of "20260103000000_late.sql"
          "-- why\n-- rowtype-migrate: no transaction\nselect 1;\n"));
  Alcotest.(check bool)
    "one it does not know is refused" false
    (Result.is_ok
       (kind_of "20260104000000_typo.sql"
          "-- rowtype-migrate: no transactions\nselect 1;\n"))

(* A baseline stands for everything before it, so a list holds one, and it
   is the oldest. *)
let test_a_baseline_is_the_oldest_and_the_only_one () =
  let dir = temp_dir () in
  let baseline stamp =
    file dir (stamp ^ "_baseline.sql")
      "-- rowtype-migrate: baseline\nselect 1;\n"
  and plain stamp = file dir (stamp ^ "_plain.sql") "select 1;\n" in
  let read paths =
    match C.of_files paths with
    | Ok ms ->
        Ok
          (List.map
             (fun (m : C.migration) ->
               match m.kind with
               | C.Baseline -> "baseline"
               | C.Transaction | C.No_transaction -> "plain")
             ms)
    | Error e -> Error (words e)
  in
  let first = baseline "20260101000000" and later = plain "20260102000000" in
  Alcotest.(check (result (list string) string))
    "the oldest"
    (Ok [ "baseline"; "plain" ])
    (read [ later; first ]);
  Alcotest.(check bool)
    "one after a migration" false
    (Result.is_ok (read [ plain "20260100000000"; first ]));
  Alcotest.(check bool)
    "two of them" false
    (Result.is_ok (read [ first; baseline "20260103000000" ]))

(* 2026-09-25 12:34:56 UTC, and a second later. *)
let at seconds =
  match Ptime.of_float_s seconds with
  | Some t -> t
  | None -> Alcotest.failf "%f is no instant" seconds

let noon = at 1_790_339_696.
let a_second_later = at 1_790_339_697.

let test_a_new_migration_is_stamped_in_utc () =
  let dir = temp_dir () in
  match C.new_migration ~dir ~now:noon "add_ratings" with
  | Error e -> Alcotest.fail (words e)
  | Ok path ->
      check_string "named for the second, in UTC"
        (Filename.concat dir "20260925123456_add_ratings.sql")
        path;
      Alcotest.(check bool) "and there" true (Sys.file_exists path)

(* A project's first migration is its first command: the directory is made,
   and a file where it would go is refused. *)
let test_a_new_migration_makes_its_directory () =
  let root = temp_dir () in
  let dir = Filename.concat (Filename.concat root "db") "migrations" in
  at_exit (fun () ->
      Array.iter (fun f -> Sys.remove (Filename.concat dir f)) (Sys.readdir dir);
      Sys.rmdir dir;
      Sys.rmdir (Filename.dirname dir));
  (match C.new_migration ~dir ~now:noon "create_users" with
  | Error e -> Alcotest.fail (words e)
  | Ok path ->
      Alcotest.(check bool) "made, and there" true (Sys.file_exists path));
  Alcotest.(check bool)
    "a file where the directory would be" false
    (Result.is_ok
       (C.new_migration ~dir:(file root "taken" "") ~now:noon "create_users"))

let test_a_new_migration_refuses_what_would_collide () =
  let dir = temp_dir () in
  let refused name ~now =
    match C.new_migration ~dir ~now name with Ok _ -> false | Error _ -> true
  in
  Alcotest.(check bool) "a name with a capital" true (refused "Add" ~now:noon);
  Alcotest.(check bool) "a name with a space" true (refused "a b" ~now:noon);
  Alcotest.(check bool) "no name" true (refused "" ~now:noon);
  Alcotest.(check bool) "the first" false (refused "one" ~now:noon);
  Alcotest.(check bool)
    "a second in the same second: one version, two files" true
    (refused "two" ~now:noon);
  Alcotest.(check bool)
    "a second later" false
    (refused "two" ~now:a_second_later)

let version n =
  match C.version_of_int n with
  | Some v -> v
  | None -> Alcotest.failf "%d is not a version" n

(* A squash is through one of the files, and has something before it to
   replace; both are refused before the server is asked anything, which is
   why the URL names no server. *)
let test_a_squash_refuses_what_it_cannot_squash () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let squash ~through migrations =
    match
      C.squash ~sw ~net:(Eio.Stdenv.net env)
        ~mono_clock:(Eio.Stdenv.mono_clock env)
        ~pg_dump:"pg_dump" ~restrict_key:"rowtype" ~through:(version through)
        ~migrations "postgres://nowhere.invalid/x"
    with
    | Ok _ -> "squashed"
    | Error (`Invalid _) -> "invalid"
    | Error (`Files _) -> "files"
    | Error e -> words e
  in
  let m n name kind = { C.version = version n; name; kind; sql = "select 1" } in
  let first = m 20260101000000 "first" C.Transaction
  and second = m 20260102000000 "second" C.Transaction
  and baseline = m 20260101000000 "baseline" C.Baseline in
  Alcotest.(check string)
    "through no migration's version" "invalid"
    (squash ~through:20260103000000 [ first; second ]);
  Alcotest.(check string)
    "through a baseline with nothing before it" "invalid"
    (squash ~through:20260101000000 [ baseline; second ]);
  Alcotest.(check string)
    "a list whose baseline is not the oldest" "files"
    (squash ~through:20260102000000
       [ m 20260100000000 "older" C.Transaction; baseline; second ])

let () =
  Alcotest.run "migrate files"
    [
      ( "files",
        [
          Alcotest.test_case "read in version order" `Quick
            test_files_are_read_in_version_order;
          Alcotest.test_case "a directive is the first line" `Quick
            test_a_directive_is_the_first_line;
          Alcotest.test_case "a baseline is the oldest and the only one" `Quick
            test_a_baseline_is_the_oldest_and_the_only_one;
        ] );
      ( "squash",
        [
          Alcotest.test_case "it refuses what it cannot squash" `Quick
            test_a_squash_refuses_what_it_cannot_squash;
        ] );
      ( "migrations",
        [
          Alcotest.test_case "a new one is stamped in UTC" `Quick
            test_a_new_migration_is_stamped_in_utc;
          Alcotest.test_case "a new one makes its directory" `Quick
            test_a_new_migration_makes_its_directory;
          Alcotest.test_case "a new one refuses what would collide" `Quick
            test_a_new_migration_refuses_what_would_collide;
        ] );
    ]

module S = Rowtype
module Sq = Rowtype_sqlite

let add_account =
  S.find
    ~params:S.(t2 text int)
    ~row:S.int
    "insert into accounts (email, balance) values ($1, $2) returning id"

let schema =
  "drop table if exists accounts; create table accounts (id integer primary \
   key, email text not null unique, balance integer not null) strict"

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let ( let* ) = Result.bind in
  match
    let* pool =
      Sq.Pool.create ~sw ~mono_clock:(Eio.Stdenv.mono_clock env) "accounts.db"
    in
    Result.join @@ Sq.Pool.use pool
    @@ fun db ->
    let* () = Sq.exec_raw db "pragma journal_mode = wal" in
    let* () = Sq.exec_raw db schema in
    let* () =
      Sq.verify db
        [
          S.Any add_account;
          S.Any Sqlite_take.balance_of;
          S.Any Sqlite_take.withdraw;
        ]
    in
    let* ada = Sq.run db add_account ("ada@example.com", 100) in
    (match Sq.run db add_account ("ada@example.com", 0) with
    | Error (`Conflict (Some columns)) -> Printf.printf "taken: %s\n" columns
    | Ok _ | Error _ ->
        prerr_endline "a second account took the same email";
        exit 1);
    let* () = Sqlite_take.take db ada 30 in
    Sq.run db Sqlite_take.balance_of ada
  with
  | Ok balance -> Printf.printf "balance: %d\n" balance
  | Error `Insufficient ->
      prerr_endline "insufficient";
      exit 1
  | Error (#Sq.error as e) ->
      prerr_endline (Sq.error_to_string e);
      exit 1

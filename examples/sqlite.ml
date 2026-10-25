module Sq = Rowtype_sqlite

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let ( let* ) = Result.bind in
  match
    let* db =
      Sq.connect ~sw ~mono_clock:(Eio.Stdenv.mono_clock env) "shop.db"
    in
    let* () =
      Sq.exec_raw db
        "create table if not exists users (id integer primary key, name text \
         not null) strict"
    in
    let* id = Sq.run db Users.add_user "Ada" in
    Sq.run db Users.find_user id
  with
  | Ok (Some u) -> print_endline u.name
  | Ok None -> print_endline "no such user"
  | Error e ->
      prerr_endline (Sq.error_to_string e);
      exit 1

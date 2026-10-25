module S = Rowtype
module Pg = Rowtype_postgres

type user = { id : int; name : string }

let user =
  S.map (S.t2 S.int S.text)
    ~parse:(fun (id, name) -> { id; name })
    ~print:(fun u -> (u.id, u.name))

let add_user =
  S.find ~params:S.text ~row:S.int
    "insert into users (name) values ($1) returning id"

let find_user =
  S.find_opt ~params:S.int ~row:user "select id, name from users where id = $1"

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let ( let* ) = Result.bind in
  match Pg.Conninfo.of_string "postgres://app@localhost/shop" with
  | Error m -> prerr_endline m
  | Ok target -> (
      match
        let* db =
          Pg.connect ~sw ~net:(Eio.Stdenv.net env)
            ~mono_clock:(Eio.Stdenv.mono_clock env)
            target
        in
        let* id = Pg.run db add_user "Ada" in
        Pg.run db find_user id
      with
      | Ok (Some u) -> print_endline u.name
      | Ok None -> print_endline "no such user"
      | Error e -> prerr_endline (S.error_to_string e))

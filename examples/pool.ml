module S = Rowtype
module Pg = Rowtype_postgres

let count_users = S.find ~params:S.unit ~row:S.int "select count(*) from users"

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  match Pg.Conninfo.of_string "postgres://app@localhost/shop" with
  | Error m -> prerr_endline m
  | Ok target -> (
      let ( let* ) = Result.bind in
      match
        let* pool =
          Pg.Pool.create ~sw ~net:(Eio.Stdenv.net env)
            ~mono_clock:(Eio.Stdenv.mono_clock env)
            ~size:16 ~observe:Counting.observe target
        in
        let users =
          Result.join (Pg.Pool.use pool (fun db -> Pg.run db count_users ()))
        in
        Pg.Pool.close pool;
        users
      with
      | Ok n ->
          Printf.printf "%d users, in %d statements\n" n
            (Atomic.get Counting.statements)
      | Error e -> prerr_endline (Pg.error_to_string e))

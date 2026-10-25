module Pg = Rowtype_postgres

let ( let* ) = Result.bind

let rec hear listener =
  let* event = Pg.Listener.next listener in
  (match (event : Pg.Listener.event) with
  | Notification { channel; payload } ->
      Printf.printf "%s: %s\n%!" channel payload
  | Reconnected -> print_endline "reconnected: what was sent meanwhile is gone");
  hear listener

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  match Pg.Conninfo.of_string "postgres://app@localhost/shop" with
  | Error m -> prerr_endline m
  | Ok target -> (
      match
        let* listener =
          Pg.Listener.connect ~sw ~net:(Eio.Stdenv.net env)
            ~mono_clock:(Eio.Stdenv.mono_clock env)
            target
        in
        let* () = Pg.Listener.listen listener "orders" in
        hear listener
      with
      | Ok () -> ()
      | Error e -> prerr_endline (Pg.error_to_string e))

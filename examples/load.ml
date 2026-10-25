module S = Rowtype
module Pg = Rowtype_postgres

let load db users =
  Pg.copy_in db ~table:"users" ~columns:[ "name"; "email" ]
    S.(t2 text text)
    (List.to_seq users)

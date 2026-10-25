module Pg = Rowtype_postgres

let claimed db ~owner order =
  Pg.run db Orders.claim (owner, order)
  |> Result.map (fun changed -> changed = 1)

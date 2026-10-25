module Pg = Rowtype_postgres

let spent db customer =
  Pg.fold db Orders.of_customer customer ~init:0 (fun sum (_id, total) ->
      sum + total)

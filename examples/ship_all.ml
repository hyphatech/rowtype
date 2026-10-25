module Pg = Rowtype_postgres

let ship_all db ids = Pg.run_many db Orders.ship ids

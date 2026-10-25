module S = Rowtype
module Pg = Rowtype_postgres

let add_user =
  S.exec
    ~params:S.(t2 text text)
    "insert into users (name, email) values ($1, $2)"

let add_each db users = Pg.run_many db add_user users

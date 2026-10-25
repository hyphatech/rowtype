module S = Rowtype
module Pg = Rowtype_postgres

let add_users =
  S.exec
    ~params:S.(t2 (array text) (array text))
    "insert into users (name, email) select * from unnest($1::text[], \
     $2::text[])"

let add_all db users = Pg.run db add_users (List.split users)

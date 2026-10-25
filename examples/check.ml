module S = Rowtype
module Pg = Rowtype_postgres

let statements =
  [
    S.Any Orders.ship;
    S.Any Orders.count;
    S.Any Orders.find;
    S.Any Orders.of_customer;
    S.Any Orders.claim;
  ]

let check db = Pg.verify db statements

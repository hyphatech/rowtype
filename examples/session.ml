module Pg = Rowtype_postgres

let set_up db =
  Pg.exec_raw db "set statement_timeout = '5s'; set application_name = 'shop'"

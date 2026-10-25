module Pg = Rowtype_postgres

let statements = Atomic.make 0

let observe : Pg.observer =
  {
    around =
      (fun _sql run ->
        Atomic.incr statements;
        run ());
  }

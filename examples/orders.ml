module S = Rowtype

let ship = S.exec ~params:S.int "update orders set shipped = true where id = $1"

let count =
  S.find ~params:S.int ~row:S.int
    "select count(*) from orders where customer_id = $1"

let find =
  S.find_opt ~params:S.int
    ~row:S.(t2 int (opt text))
    "select id, note from orders where id = $1"

let of_customer =
  S.list ~params:S.int
    ~row:S.(t2 int int)
    "select id, total from orders where customer_id = $1 order by id"

let claim =
  S.exec_count
    ~params:S.(t2 text int)
    "update orders set owner = $1 where id = $2 and owner is null"

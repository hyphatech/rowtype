module S = Rowtype
module Sq = Rowtype_sqlite

let balance_of =
  S.find ~params:S.int ~row:S.int "select balance from accounts where id = $1"

let withdraw =
  S.exec
    ~params:S.(t2 int int)
    "update accounts set balance = balance - $2 where id = $1"

let ( let* ) = Result.bind

let take db account amount =
  Sq.Transaction.within db ~behaviour:Immediate ~retries:3 (fun db ->
      let* balance = Sq.run db balance_of account in
      if balance < amount then Error `Insufficient
      else Sq.run db withdraw (account, amount))

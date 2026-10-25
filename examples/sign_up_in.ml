module Pg = Rowtype_postgres

let sign_up_in pool email =
  match
    Result.join
      (Pg.Pool.use pool (fun db ->
           Pg.Transaction.within db (fun db ->
               Pg.run db Sign_up.add_account email)))
  with
  | Ok id -> Some id
  | Error e ->
      Logs.err (fun m -> m "sign-up: %s" (Pg.error_to_string e));
      None

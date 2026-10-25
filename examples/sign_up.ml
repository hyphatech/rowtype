module S = Rowtype
module Pg = Rowtype_postgres

let add_account =
  S.find ~params:S.text ~row:S.int
    "insert into accounts (email) values ($1) returning id"

type answer = Created of int | Taken | Try_again | Unknown | Failed

let sign_up db email =
  match Pg.run db add_account email with
  | Ok id -> Created id
  | Error (`Conflict (Some "accounts_email_key")) -> Taken
  | Error (`Unsent _ | `Not_serializable _) -> Try_again
  | Error (`Lost _) -> Unknown
  | Error e ->
      Logs.err (fun m -> m "sign-up: %s" (S.error_to_string e));
      Failed

module S = Rowtype
module Pg = Rowtype_postgres

let add_member =
  S.find ~params:S.text ~row:S.int
    "insert into members (name) values ($1) returning id"

type error =
  | Empty_name
  | Taken of string option
  | Failed of string
  | Unavailable of Pg.Transaction.failure

let work db name =
  if String.equal name "" then Error Empty_name
  else
    match Pg.run db add_member name with
    | Ok id -> Ok id
    | Error (`Conflict name) -> Error (Taken name)
    | Error e -> Error (Failed (S.error_to_string e))

let join db name =
  match
    Pg.Transaction.within db (fun db ->
        Result.map_error (fun e -> `Work e) (work db name))
  with
  | Ok v -> Ok v
  | Error (`Work e) -> Error e
  | Error (`Conflict name) -> Error (Taken name)
  | Error (#Pg.Transaction.failure as f) -> Error (Unavailable f)

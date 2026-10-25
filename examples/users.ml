module S = Rowtype

type user = { id : int; name : string }

let user =
  S.map (S.t2 S.int S.text)
    ~parse:(fun (id, name) -> { id; name })
    ~print:(fun u -> (u.id, u.name))

let add_user =
  S.find ~params:S.text ~row:S.int
    "insert into users (name) values ($1) returning id"

let find_user =
  S.find_opt ~params:S.int ~row:user "select id, name from users where id = $1"

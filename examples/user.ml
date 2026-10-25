module S = Rowtype

type user = { id : int; name : string }

let user =
  S.map (S.t2 S.int S.text)
    ~parse:(fun (id, name) -> { id; name })
    ~print:(fun u -> (u.id, u.name))

module S = Rowtype

type t = string

let of_string s =
  let upper_or_digit = function
    | 'A' .. 'Z' | '0' .. '9' -> true
    | _ -> false
  in
  if String.length s = 8 && String.for_all upper_or_digit s then Ok s
  else Error "not a SKU, which is eight capitals or digits"

let to_string s = s
let sku = S.parse S.text ~parse:of_string ~print:to_string

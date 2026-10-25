module S = Rowtype

let find =
  S.find_opt ~params:Sku.sku
    ~row:S.(t3 text Colour.colour (opt text))
    "select name, colour, note from products where sku = $1"

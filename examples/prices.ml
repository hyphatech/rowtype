module S = Rowtype

let price_of =
  S.find ~params:Sku.sku ~row:S.text
    "select price::text from products where sku = $1"

let set_price =
  S.exec
    ~params:S.(t2 text Sku.sku)
    "update products set price = $1::text::numeric where sku = $2"

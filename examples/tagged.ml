module S = Rowtype

let tagged =
  S.list ~params:S.text
    ~row:S.(t2 Sku.sku (array text))
    "select sku, tags from products where $1 = any(tags) order by sku"

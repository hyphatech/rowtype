# Columns and types

One shape describes a statement's parameters or its rows, and drives both
directions: binding the values going in and decoding the rows coming out.
A shape is built from the scalars below and the tuples over them.

## The scalars

| Column | Postgres | OCaml |
|---|---|---|
| `S.int` | `int2`, `int4`, `int8`, `oid` | `int`; an `int8` past 63 bits is refused |
| `S.int64` | `int2`, `int4`, `int8`, `oid` | `int64`, the whole of an `int8` |
| `S.float` | `float4`, `float8` | `float` |
| `S.text` | `text`, `varchar`, `char`, `name`, `uuid`, `json`, `jsonb` | `string` |
| `S.bytes` | `bytea` | `string` |
| `S.bool` | `bool` | `bool` |
| `S.instant` | `timestamptz` | `Ptime.t`, kept to the microsecond; years 1 to 9999, and infinity is refused |
| `S.date` | `date` | `Ptime.date`; years 1 to 9999, and infinity is refused |
| `S.timestamp` | `timestamp` | `Ptime.t`, the instant whose reading in UTC it is; years 1 to 9999, and infinity is refused |
| `S.interval` | `interval` | `S.interval`: months, days and microseconds, each apart; infinity is refused |
| `S.uuid` | `uuid` | `Uuidm.t` |
| `S.json` | `json`, `jsonb` | `string`, the document |
| `S.enum label values` | an enum | each of `values`, written as its `label` |

Every scalar refuses a NULL; a column that may hold one is `S.opt`. An
instant is sent as ISO 8601 in UTC and read back whatever the session's
time zone. A value its scalar cannot hold is refused as what it is, saying
what reads it instead. SQLite keeps each scalar its own way, in
[its own table](sqlite.md#how-each-column-is-kept).

An interval is months, days and microseconds apart, each signed on its own,
since a month is no fixed number of days and a day, across a change of
clocks, no fixed number of hours. A JSON document is its text; parsing it
is yours.

## Tuples

`S.t2` to `S.t11` combine shapes into a row or a list of parameters. They
are flat, so a call site never sees the nesting: `S.t3 S.int S.text S.bool`
is an `int * string * bool`.

## Your own types

`S.map` or `S.parse` read a column as your own type: `~parse` from the
column's, and `~print` back.

```ocaml title="user.ml"
module S = Rowtype

type user = { id : int; name : string }

let user =
  S.map (S.t2 S.int S.text)
    ~parse:(fun (id, name) -> { id; name })
    ~print:(fun u -> (u.id, u.name))
```

`S.parse`'s `~parse` may refuse a value, answering `Error` with a sentence
that says what it is not, which the row's error gives after the column's
name:

```ocaml title="sku.ml"
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
```

The sentence never holds the value, since a failure reaches a log.

## Enums

An enum is read and written through the labels of your own type:

```ocaml title="colour.ml"
type t = Red | Green | Blue

let all = [ Red; Green; Blue ]
let to_string = function Red -> "red" | Green -> "green" | Blue -> "blue"
let colour = Rowtype.enum to_string all
```

A row holding a label none of `values` has is refused, naming its column.
`Pg.verify` holds the column to an enum that has every one of the labels, so
a migration may add a label before the code that reads it ships. It refuses
an enum whose list is empty or gives two values one label.

Shapes of your own types combine as any others:

```ocaml title="products.ml"
module S = Rowtype

let find =
  S.find_opt ~params:Sku.sku
    ~row:S.(t3 text Colour.colour (opt text))
    "select name, colour, note from products where sku = $1"
```

## NULL

`S.opt` is a NULL. `None` binds NULL in every column the shape spans, and
decodes from a row where every one of them is NULL. A value that is itself
NULL in every column -- `Some None` of `S.opt (S.opt S.int)` -- reads back
as `None`, since a row cannot tell the two apart.

## Arrays

`S.array` is a Postgres array, a column holding a list, each value of the
element's shape: `S.array S.int` for a `smallint[]`, `S.array (S.opt
S.float)` for a `real[]` that holds NULLs, `S.array sku` for values read
through their own `S.parse`.

```ocaml title="tagged.ml"
module S = Rowtype

let tagged =
  S.list ~params:S.text
    ~row:S.(t2 Sku.sku (array text))
    "select sku, tags from products where $1 = any(tags) order by sku"
```

An element is one column and no array of its own: one dimension, as a list
has. A list has no first index, so an array is read in order from wherever
the database starts it. A list bound as an array also writes many rows in
one statement: see [bulk writes](bulk-writes.md).

## Any other type

Any other type -- `numeric`, `time`, a range, `inet` -- is read through a
cast in the SQL (`price::text`, `spans::text[]`) and `S.text`, and a read
without one is refused, saying so. It is written from `S.text` the same way,
cast to text and then to its type: `$1::text::numeric`, `$1::text::time`,
`$1::text::inet`. `$1::numeric` alone makes the parameter a `numeric`, which
`Pg.verify` refuses for a text.

```ocaml title="prices.ml"
module S = Rowtype

let price_of =
  S.find ~params:Sku.sku ~row:S.text
    "select price::text from products where sku = $1"

let set_price =
  S.exec
    ~params:S.(t2 text Sku.sku)
    "update products set price = $1::text::numeric where sku = $2"
```

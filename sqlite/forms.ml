module D = Sqlite3.Data

let ( let* ) = Option.bind
let is_digit c = match c with '0' .. '9' -> true | _ -> false

(* The [n] digits at [i], and nothing else there: neither a sign nor a
   space, which [int_of_string] would take. *)
let digits s i n =
  if i + n <= String.length s && String.for_all is_digit (String.sub s i n) then
    int_of_string_opt (String.sub s i n)
  else None

let at s i c = i < String.length s && Char.equal s.[i] c

(* ------------------------------------------------------------------ *)
(* Dates and times *)

let microseconds t =
  let _, ps = Ptime.Span.to_d_ps (Ptime.frac_s t) in
  Int64.to_int (Int64.div ps 1_000_000L)

let instant t =
  let (y, mo, d), ((h, mi, s), _) = Ptime.to_date_time t in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d.%06d" y mo d h mi s
    (microseconds t)

let date (y, mo, d) = Printf.sprintf "%04d-%02d-%02d" y mo d

let date_of_string s =
  if String.length s <> 10 || not (at s 4 '-' && at s 7 '-') then None
  else
    let* y = digits s 0 4 in
    let* mo = digits s 5 2 in
    let* d = digits s 8 2 in
    Option.map (fun _ -> (y, mo, d)) (Ptime.of_date (y, mo, d))

(* A fraction's digits as picoseconds, Ptime's unit, kept to the
   microsecond an instant is written to, so one read is one that writes
   back as itself, and compares with one written. *)
let picoseconds fraction =
  let kept = String.sub fraction 0 (min 6 (String.length fraction)) in
  Option.map
    (fun us -> Int64.mul (Int64.of_int us) 1_000_000L)
    (int_of_string_opt (kept ^ String.make (6 - String.length kept) '0'))

(* Only text SQLite's functions read too, or a column that SQL compares as
   NULL would be an instant here: seconds to 59, a fraction only after
   them, and a zone to 14 hours and its minutes to 59. *)
let date_time s =
  let n = String.length s in
  let* date = if n >= 10 then date_of_string (String.sub s 0 10) else None in
  if (not (at s 10 ' ' || at s 10 'T')) || not (at s 13 ':') then None
  else
    let* h = digits s 11 2 in
    let* mi = digits s 14 2 in
    let* sec, i =
      if at s 16 ':' then
        match digits s 17 2 with
        | Some sec when sec <= 59 -> Some (sec, 19)
        | Some _ | None -> None
      else Some (0, 16)
    in
    let* ps, i =
      if at s i '.' && i = 19 then
        let rec last j = if j < n && is_digit s.[j] then last (j + 1) else j in
        let j = last (i + 1) in
        if j = i + 1 then None
        else
          Option.map
            (fun ps -> (ps, j))
            (picoseconds (String.sub s (i + 1) (j - i - 1)))
      else Some (0L, i)
    in
    let* zone =
      if i = n then Some None
      else if at s i 'Z' && i + 1 = n then Some (Some 0)
      else if (at s i '+' || at s i '-') && i + 6 = n && at s (i + 3) ':' then
        let* zh = digits s (i + 1) 2 in
        let* zm = digits s (i + 4) 2 in
        if zh > 14 || zm > 59 then None
        else
          let offset = (zh * 3600) + (zm * 60) in
          Some (Some (if at s i '-' then -offset else offset))
      else None
    in
    let* t =
      Ptime.of_date_time (date, ((h, mi, sec), Option.value zone ~default:0))
    in
    let* t = Ptime.add_span t (Ptime.Span.v (0, ps)) in
    Some (t, zone)

(* ------------------------------------------------------------------ *)
(* Intervals *)

let interval (i : Rowtype.interval) =
  (* Division truncates towards zero, so both parts of a negative count
     are negative, and negated each is a part to write; negated whole,
     min_int would stay negative. *)
  let seconds =
    let us = i.microseconds in
    if us < 0 then
      Printf.sprintf "-%d.%06d" (-(us / 1_000_000)) (-(us mod 1_000_000))
    else Printf.sprintf "%d.%06d" (us / 1_000_000) (us mod 1_000_000)
  in
  Printf.sprintf "P%dM%dDT%sS" i.months i.days seconds

(* An optional minus and digits, which [int_of_string] alone would take
   with a plus, an underscore or a 0x. *)
let integer s =
  let body =
    if String.length s > 0 && Char.equal s.[0] '-' then
      String.sub s 1 (String.length s - 1)
    else s
  in
  if String.length body > 0 && String.for_all is_digit body then
    int_of_string_opt s
  else None

(* Seconds as microseconds, counted in [int64] where a whole second's
   million fits past an [int]'s range, then held to it. *)
let microseconds_of_seconds s =
  let negative = String.length s > 0 && Char.equal s.[0] '-' in
  let s = if negative then String.sub s 1 (String.length s - 1) else s in
  let whole, fraction =
    match String.index_opt s '.' with
    | None -> (s, "")
    | Some p ->
        (String.sub s 0 p, String.sub s (p + 1) (String.length s - p - 1))
  in
  if
    String.length whole = 0
    || (not (String.for_all is_digit whole))
    || String.length fraction > 6
    || (not (String.for_all is_digit fraction))
    || (String.length fraction = 0 && String.contains s '.')
  then None
  else
    let* whole = int_of_string_opt whole in
    (* A day of seconds a million times over is far below this, and
       anything above it overflows the [int64] it is counted in. *)
    if whole > 9_000_000_000_000 then None
    else
      let fraction =
        int_of_string_opt
          (fraction ^ String.make (6 - String.length fraction) '0')
      in
      let* fraction = fraction in
      let us =
        Int64.add
          (Int64.mul (Int64.of_int whole) 1_000_000L)
          (Int64.of_int fraction)
      in
      let us = if negative then Int64.neg us else us in
      if
        Int64.compare us (Int64.of_int min_int) < 0
        || Int64.compare us (Int64.of_int max_int) > 0
      then None
      else Some (Int64.to_int us)

let interval_of_string s =
  let n = String.length s in
  let part from stop =
    match String.index_from_opt s from stop with
    | Some j -> Some (String.sub s from (j - from), j + 1)
    | None -> None
  in
  if n < 2 || not (Char.equal s.[0] 'P' && Char.equal s.[n - 1] 'S') then None
  else
    let* months, i = part 1 'M' in
    let* days, i = part i 'D' in
    if not (at s i 'T') then None
    else
      let* months = integer months in
      let* days = integer days in
      let* microseconds =
        microseconds_of_seconds (String.sub s (i + 1) (n - i - 2))
      in
      Some { Rowtype.months; days; microseconds }

(* ------------------------------------------------------------------ *)
(* Bytes *)

let hex bytes =
  let b = Buffer.create (2 * String.length bytes) in
  String.iter
    (fun c -> Buffer.add_string b (Printf.sprintf "%02x" (Char.code c)))
    bytes;
  Buffer.contents b

let of_hex s =
  let value c =
    match c with
    | '0' .. '9' -> Some (Char.code c - Char.code '0')
    | 'a' .. 'f' -> Some (Char.code c - Char.code 'a' + 10)
    | 'A' .. 'F' -> Some (Char.code c - Char.code 'A' + 10)
    | _ -> None
  in
  let n = String.length s in
  if n mod 2 <> 0 then None
  else
    let b = Bytes.create (n / 2) in
    let rec go i =
      if i = n / 2 then Some (Bytes.to_string b)
      else
        let* hi = value s.[2 * i] in
        let* lo = value s.[(2 * i) + 1] in
        Bytes.set b i (Char.chr ((hi * 16) + lo));
        go (i + 1)
    in
    go 0

(* ------------------------------------------------------------------ *)
(* Arrays, as JSON *)

(* A quote, a backslash and a control character escaped, which is all
   JSON asks; any other byte is as it was. *)
let quoted s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | '\r' -> Buffer.add_string b "\\r"
      | '\t' -> Buffer.add_string b "\\t"
      | c when Char.code c < 0x20 ->
          Buffer.add_string b (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b

(* The shortest of the two forms that reads back as the float it was. *)
let number f =
  if Float.is_nan f then "null"
  else if Float.equal f Float.infinity then "9e999"
  else if Float.equal f Float.neg_infinity then "-9e999"
  else
    let short = Printf.sprintf "%.15g" f in
    match float_of_string_opt short with
    | Some g when Float.equal g f -> short
    | Some _ | None -> Printf.sprintf "%.17g" f

let array values =
  let element = function
    | D.INT i -> Int64.to_string i
    | D.FLOAT f -> number f
    | D.TEXT s -> quoted s
    | D.BLOB b -> quoted (hex b)
    | D.NULL | D.NONE -> "null"
  in
  "[" ^ String.concat "," (List.map element values) ^ "]"

(* A reader over the text, one element at a time, which refuses by
   raising where it stands and answers a value at the edge. *)
let elements s =
  let exception Refused of [ `Not_an_array | `Nested ] in
  let n = String.length s in
  let pos = ref 0 in
  let peek () = if !pos < n then Some s.[!pos] else None in
  let refuse () = raise (Refused `Not_an_array) in
  let rec space () =
    match peek () with
    | Some (' ' | '\t' | '\n' | '\r') ->
        incr pos;
        space ()
    | Some _ | None -> ()
  in
  let expect c =
    if !pos < n && Char.equal s.[!pos] c then incr pos else refuse ()
  in
  let word w v =
    if
      !pos + String.length w <= n
      && String.equal (String.sub s !pos (String.length w)) w
    then (
      pos := !pos + String.length w;
      v)
    else refuse ()
  in
  let hex4 () =
    let digits = if !pos + 4 <= n then String.sub s !pos 4 else "" in
    match of_hex digits with
    | Some two when String.length two = 2 ->
        pos := !pos + 4;
        (Char.code two.[0] * 256) + Char.code two.[1]
    | Some _ | None -> refuse ()
  in
  let string () =
    expect '"';
    let b = Buffer.create 16 in
    let rec go () =
      match peek () with
      | None -> refuse ()
      | Some '"' -> incr pos
      | Some '\\' -> (
          incr pos;
          let c = peek () in
          incr pos;
          match c with
          | Some '"' ->
              Buffer.add_char b '"';
              go ()
          | Some '\\' ->
              Buffer.add_char b '\\';
              go ()
          | Some '/' ->
              Buffer.add_char b '/';
              go ()
          | Some 'b' ->
              Buffer.add_char b '\b';
              go ()
          | Some 'f' ->
              Buffer.add_char b '\012';
              go ()
          | Some 'n' ->
              Buffer.add_char b '\n';
              go ()
          | Some 'r' ->
              Buffer.add_char b '\r';
              go ()
          | Some 't' ->
              Buffer.add_char b '\t';
              go ()
          | Some 'u' ->
              let u = hex4 () in
              (* A character past the first plane is two escapes, a high
                 surrogate and then a low one. *)
              let u =
                if u >= 0xD800 && u <= 0xDBFF then (
                  expect '\\';
                  expect 'u';
                  let low = hex4 () in
                  if low >= 0xDC00 && low <= 0xDFFF then
                    0x10000 + ((u - 0xD800) lsl 10) + (low - 0xDC00)
                  else refuse ())
                else if u >= 0xDC00 && u <= 0xDFFF then refuse ()
                else u
              in
              Buffer.add_utf_8_uchar b (Uchar.of_int u);
              go ()
          | Some _ | None -> refuse ())
      | Some c when Char.code c < 0x20 -> refuse ()
      | Some c ->
          Buffer.add_char b c;
          incr pos;
          go ()
    in
    go ();
    Buffer.contents b
  in
  (* RFC 8259's number: no leading zero, a digit each side of the point,
     and one in the exponent. *)
  let number () =
    let start = !pos in
    let digits () =
      let first = !pos in
      while !pos < n && is_digit s.[!pos] do
        incr pos
      done;
      if !pos = first then refuse ()
    in
    if Option.equal Char.equal (peek ()) (Some '-') then incr pos;
    (match peek () with Some '0' -> incr pos | Some _ | None -> digits ());
    let fraction = Option.equal Char.equal (peek ()) (Some '.') in
    if fraction then (
      incr pos;
      digits ());
    let exponent =
      match peek () with Some ('e' | 'E') -> true | Some _ | None -> false
    in
    if exponent then (
      incr pos;
      (match peek () with Some ('-' | '+') -> incr pos | Some _ | None -> ());
      digits ());
    let text = String.sub s start (!pos - start) in
    match
      (fraction || exponent, Int64.of_string_opt text, float_of_string_opt text)
    with
    | false, Some i, _ -> D.INT i
    | _, _, Some f -> D.FLOAT f
    | _, _, None -> refuse ()
  in
  let element () =
    match peek () with
    | Some '"' -> Some (D.TEXT (string ()))
    | Some 'n' -> word "null" None
    | Some 't' -> word "true" (Some (D.INT 1L))
    | Some 'f' -> word "false" (Some (D.INT 0L))
    | Some ('-' | '0' .. '9') -> Some (number ())
    | Some ('[' | '{') -> raise (Refused `Nested)
    | Some _ | None -> refuse ()
  in
  let rec rest acc =
    space ();
    let acc = element () :: acc in
    space ();
    match peek () with
    | Some ',' ->
        incr pos;
        rest acc
    | Some ']' ->
        incr pos;
        List.rev acc
    | Some _ | None -> refuse ()
  in
  match
    space ();
    expect '[';
    space ();
    let values =
      if Option.equal Char.equal (peek ()) (Some ']') then (
        incr pos;
        [])
      else rest []
    in
    space ();
    if !pos <> n then refuse ();
    values
  with
  | values -> Ok values
  | exception Refused r -> Error r

(** The text forms SQLite keeps a value in where it has no kind of value for one
    -- an instant, a date, an interval, an array -- written and read with no IO.
*)

val instant : Ptime.t -> string
(** [2026-10-20 09:24:46.621000]: in UTC, to the microsecond below, as SQLite's
    date and time functions write one, every digit of the fraction written so
    that text in this form sorts as time does. *)

val date_time : string -> (Ptime.t * Ptime.tz_offset_s option) option
(** A date and a time as SQLite's functions read one -- [YYYY-MM-DD HH:MM], a
    [T] for the space, seconds, a fraction of any number of digits, kept to the
    microsecond, and a zone, [Z] or [+HH:MM] -- and the zone it gave: the
    instant in that zone, or in UTC where it gave none. *)

val date : Ptime.date -> string
(** [2026-10-20]. *)

val date_of_string : string -> Ptime.date option
(** A date in {!date}'s form, and one that exists. *)

val interval : Rowtype.interval -> string
(** ISO 8601 with each part signed on its own, as an interval's are:
    [P14M3DT-0.000005S], the seconds to the microsecond. *)

val interval_of_string : string -> Rowtype.interval option
(** An interval in {!interval}'s form, the seconds' fraction of up to six digits
    or none, and one whose parts an [int] holds. *)

val hex : string -> string
(** Bytes as lowercase hex, two digits each. *)

val of_hex : string -> string option
(** Hex of either case, two digits a byte, as bytes. *)

val array : Sqlite3.Data.t list -> string
(** Values as a JSON array: an integer or a float a number, a float that is
    infinite one too large for any ([9e999], as SQLite writes one), a NaN and a
    NULL [null], text a string and a blob a string of its {!hex}. *)

val elements :
  string -> (Sqlite3.Data.t option list, [ `Not_an_array | `Nested ]) result
(** A JSON array's elements, in order, [None] for a [null]: a number without a
    fraction or an exponent an integer where one holds it, any other a float, a
    string text, and [true] and [false] the integers [1] and [0]. [`Nested] for
    an element that is an array or an object, [`Not_an_array] for any other text
    that is not one. *)

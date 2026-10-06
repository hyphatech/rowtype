(* rowtype over Postgres, through postgres-eio: every parameter text, and
   a result binary wherever the driver reads its type and the connection
   keeps statements, so a scalar is sent as the driver's text form of it
   and read by the driver's decoder for it, whichever format the cell came
   in. *)

module S = Rowtype
module Pg = Postgres_eio
module Conninfo = Postgres_eio.Conninfo

let ( let* ) = Result.bind

type observer = { around : 'a. string -> (unit -> 'a) -> 'a }

let unobserved = { around = (fun _ f -> f ()) }

(* Whether its owner closed it. The driver says only that a connection is
   closed, by [close] or by a failure alike; one a failure closed is made
   again before a transaction, and one its owner closed is not. *)
type ownership = Kept | Closed_by_owner

(* A connection, what watches the statements it runs, which a pool hands
   to every connection it lends, and the monotonic clock a statement is
   timed on and a cancelled one's request bounded by, which a wall clock
   that jumps cannot move. *)
type conn = {
  pg : Pg.t;
  observe : observer;
  clock : Eio.Time.Mono.ty Eio.Std.r;
  mutable ownership : ownership;
}

let src =
  Logs.Src.create "rowtype-postgres" ~doc:"Each statement, and how long it took"

module Log = (val Logs.src_log src : Logs.LOG)

(* A statement's first line, which is enough to find it where it was
   written. *)
let first_line sql =
  match String.split_on_char '\n' (String.trim sql) with
  | line :: _ -> String.trim line
  | [] -> ""

(* What a statement was and how long it took -- the text, which has
   placeholders where the values are, and never the parameters: a code, a
   digest or a token is a parameter. *)
let timed t sql f =
  t.observe.around sql @@ fun () ->
  let started = Eio.Time.Mono.now t.clock in
  let v = f () in
  Log.debug (fun m ->
      m "%s  (%.1f ms)" (first_line sql)
        (Mtime.Span.to_float_ns (Mtime.span started (Eio.Time.Mono.now t.clock))
        /. 1e6));
  v

(* A data exception, class 22, is the one class whose message quotes the
   value it refused -- [invalid input syntax for type integer: "..."] --
   and a value may be a token, so its words are the code's, not the
   server's. *)
let db_error = function
  | Pg.Server e
    when String.starts_with ~prefix:"22" (Pg.Server_error.sqlstate e) ->
      `Db
        (Printf.sprintf
           "ERROR %s: a value the database could not take (its words are left \
            out, since they quote the value)"
           (Pg.Server_error.sqlstate e))
  | e -> `Db (Pg.error_to_string e)

(* The built-in types by their oids in pg_type, which are fixed: what the
   server writes for a column's type in a description. *)
let oid_bool = 16
let oid_bytea = 17
let oid_name = 19
let oid_int8 = 20
let oid_int2 = 21
let oid_int4 = 23
let oid_text = 25
let oid_oid = 26
let oid_json = 114
let oid_float4 = 700
let oid_float8 = 701
let oid_unknown = 705
let oid_bpchar = 1042
let oid_varchar = 1043
let oid_date = 1082
let oid_timestamp = 1114
let oid_timestamptz = 1184
let oid_interval = 1186
let oid_uuid = 2950
let oid_jsonb = 3802

(* Below this oid every type is Postgres's own, fixed in pg_type; from it
   on, a type is one a database made -- an enum, an extension's type, an
   array of either -- which no oid alone describes (FirstNormalObjectId in
   Postgres's transam.h). *)
let first_user_oid = 16384

(* Which of Postgres's own types each scalar reads: the integers and oid,
   the two floats, text of every kind and what is read as text -- a name, a
   uuid, JSON -- and one each for the rest. A statement's check and a read
   both ask this, so what one accepts the other reads. *)
let reads_builtin : type a. a S.scalar -> int -> bool =
 fun scalar oid ->
  match scalar with
  | S.Int | S.Int64 -> List.mem oid [ oid_int8; oid_int2; oid_int4; oid_oid ]
  | S.Float -> List.mem oid [ oid_float4; oid_float8 ]
  | S.Text ->
      List.mem oid
        [
          oid_name;
          oid_text;
          oid_json;
          oid_bpchar;
          oid_varchar;
          oid_uuid;
          oid_jsonb;
        ]
  | S.Bytes -> oid = oid_bytea
  | S.Bool -> oid = oid_bool
  | S.Instant -> oid = oid_timestamptz
  | S.Date -> oid = oid_date
  | S.Timestamp -> oid = oid_timestamp
  | S.Interval -> oid = oid_interval
  | S.Uuid -> oid = oid_uuid
  | S.Json -> List.mem oid [ oid_json; oid_jsonb ]

(* What a [text] declared over a type the database made is told to do, both
   where a read refuses one and where a statement's check does. *)
let cast_hint : type a. a S.scalar -> int -> string -> string =
 fun scalar oid cast ->
  match scalar with
  | S.Text when oid >= first_user_oid ->
      ": a type the database made, cast it, as " ^ cast
  | _ -> ""

(* A statement on a connection already closed is never sent, which is what
   tells a failure that did nothing from one whose statement was in flight.
   The driver answers [Closed] to both, and a connection is one fiber's, so
   asking first cannot race. *)
type failure = Unsent | Failed of Pg.error

(* How long a cancelled fiber waits for the server to take its cancel
   request, a new connection's worth of round trips: past it the fiber
   goes, and the statement runs on as it would have without asking. *)
let cancel_request_s = 5.

(* A fiber cancelled with its statement in flight leaves the statement
   running: the driver closes the socket, and the server notices only when
   it next writes. So the server is asked to cancel it, over a connection
   of its own, as a pool does for a borrower cancelled; with nothing
   running the request does nothing. *)
let send t f =
  if Pg.closed t.pg then Error Unsent
  else
    match f t.pg with
    | answer -> Result.map_error (fun e -> Failed e) answer
    | exception (Eio.Cancel.Cancelled _ as ex) ->
        let trace = Printexc.get_raw_backtrace () in
        Eio.Cancel.protect (fun () ->
            Eio.Fiber.first
              (fun () -> ignore (Pg.cancel t.pg : (unit, Pg.error) result))
              (fun () -> Eio.Time.Mono.sleep t.clock cancel_request_s));
        Printexc.raise_with_backtrace ex trace

(* A FATAL or a PANIC ends the session, and the server sends it in place
   of the statement's answer. *)
let ends_session e =
  match Pg.Server_error.severity e with
  | Pg.Server_error.Fatal | Pg.Server_error.Panic -> true
  | Pg.Server_error.Error | Warning | Notice | Debug | Info | Log | Other _ ->
      false

module Backend = struct
  type nonrec conn = conn
  type param = string option

  (* A cell and how it is read. A column the server described says its type
     and format. An array's element is text, read as a column of the
     element's type, so the driver checks it and reads it as it reads that
     type alone -- a [real] rounded to single precision, a [text] refused
     as an [int]. A column past the ones described is one no server sends,
     and is refused rather than guessed at. *)
  type cell =
    | Described of Pg.Column.t * string
    | Element of Pg.Column.t * string * Pg.Oid.t
    | Undescribed

  type nonrec failure = failure

  let null = None

  let param : type a. a S.scalar -> a -> string option =
   fun s v ->
    Some
      (match s with
      | S.Int -> Pg.Text.int v
      | S.Int64 -> Pg.Text.int64 v
      | S.Float -> Pg.Text.float v
      | S.Text -> v
      | S.Bytes -> Pg.Text.bytes v
      | S.Bool -> Pg.Text.bool v
      | S.Instant -> Pg.Text.timestamptz v
      | S.Date -> Pg.Text.date v
      | S.Timestamp -> Pg.Text.timestamp v
      | S.Interval ->
          Pg.Text.interval
            { months = v.months; days = v.days; microseconds = v.microseconds }
      | S.Uuid -> Uuidm.to_string v
      | S.Json -> v)

  (* A refusal names the column's type and never its value, which may be a
     token or an address and would reach a log; [got] says what came instead
     of a value of the scalar's type. A cell is read only where its type is
     one {!reads_builtin} gives the scalar, as a statement's check reads it:
     the driver alone reads any type it has no binary form for as its text
     says, a [numeric] as an [int], an enum as JSON. A type the database
     made is read through a cast, since no oid of one says what it is. *)
  let read_as : type a.
      a S.scalar ->
      Pg.Column.t ->
      string ->
      got:string ->
      cast:string ->
      (a, string) result =
   fun s column raw ~got ~cast ->
    let oid = Pg.Oid.to_int column.type_oid in
    let hint = cast_hint s oid cast in
    let as_ expected v =
      match v with
      | Some v when reads_builtin s oid -> Ok v
      | Some _ | None ->
          Error (Printf.sprintf "expected %s, got %s%s" expected got hint)
    in
    match s with
    | S.Int -> as_ "INT" (Pg.Value.int column raw)
    | S.Int64 -> as_ "INT8" (Pg.Value.int64 column raw)
    | S.Float -> as_ "FLOAT" (Pg.Value.float column raw)
    | S.Text -> as_ "TEXT" (Pg.Value.text column raw)
    | S.Bytes -> as_ "BYTEA" (Pg.Value.bytes column raw)
    | S.Bool -> as_ "BOOL" (Pg.Value.bool column raw)
    | S.Instant -> as_ "TIMESTAMPTZ" (Pg.Value.timestamptz column raw)
    | S.Date -> as_ "DATE" (Pg.Value.date column raw)
    | S.Timestamp -> as_ "TIMESTAMP" (Pg.Value.timestamp column raw)
    | S.Interval ->
        as_ "INTERVAL"
          (Option.map
             (fun (i : Pg.Interval.t) ->
               {
                 S.months = i.months;
                 days = i.days;
                 microseconds = i.microseconds;
               })
             (Pg.Value.interval column raw))
    | S.Uuid -> as_ "UUID" (Pg.Value.uuid column raw)
    | S.Json -> as_ "JSON" (Pg.Value.json column raw)

  let read : type a. a S.scalar -> cell -> (a, string) result =
   fun s cell ->
    match cell with
    | Described (column, raw) ->
        read_as s column raw
          ~got:
            (Printf.sprintf "a value of type %d"
               (Pg.Oid.to_int column.type_oid))
          ~cast:"::text"
    | Element (column, raw, array) ->
        read_as s column raw
          ~got:
            (Printf.sprintf "an element of an array of type %d"
               (Pg.Oid.to_int array))
          ~cast:"::text[]"
    | Undescribed -> Error "a column the server did not describe"

  let fold t sql params ~init ~row =
    timed t sql @@ fun () ->
    let columns = ref [||] in
    let cell i raw =
      if i < Array.length !columns then Described (!columns.(i), raw)
      else Undescribed
    in
    Result.map
      (fun (acc, tag) -> (acc, Option.value (Pg.Tag.rows tag) ~default:0))
      (* A Bind names each column's format, which only a statement the
         connection described and kept can say: a connection that keeps
         none, behind a pooler that cannot carry them, reads text. *)
      ( send t @@ fun pg ->
        Pg.query pg sql ~params
          ~binary:(Pg.statement_cache pg > 0)
          ~columns:(fun described -> columns := described)
          ~init
          ~row:(fun acc cells ->
            row acc (Array.mapi (fun i c -> Option.map (cell i) c) cells)) )

  let batch t sql params =
    timed t sql @@ fun () ->
    Result.map ignore (send t (fun pg -> Pg.execute_many pg sql ~params))

  let script t sql =
    timed t sql @@ fun () -> send t (fun pg -> Pg.script pg sql)

  (* Every element quoted, a quote and a backslash in it escaped, so no text
     can end it early; a NULL unquoted, which is what makes it one. *)
  let array elements =
    let quoted v =
      let b = Buffer.create (String.length v + 2) in
      Buffer.add_char b '"';
      String.iter
        (fun c ->
          if Char.equal c '"' || Char.equal c '\\' then Buffer.add_char b '\\';
          Buffer.add_char b c)
        v;
      Buffer.add_char b '"';
      Buffer.contents b
    in
    Some
      ("{"
      ^ String.concat ","
          (List.map (function None -> "NULL" | Some v -> quoted v) elements)
      ^ "}")

  (* The element type of each built-in array of a type the driver reads,
     by the oids pg_type fixes. Any other array -- of [numeric], of an enum
     -- keeps its own oid, which the driver never reads in binary, so its
     element is read as its text says, as the element's own type would be. *)
  let element_oid array =
    match Pg.Oid.to_int array with
    | 1000 -> Some oid_bool
    | 1001 -> Some oid_bytea
    | 1003 -> Some oid_name
    | 1005 -> Some oid_int2
    | 1007 -> Some oid_int4
    | 1009 -> Some oid_text
    | 1014 -> Some oid_bpchar
    | 1015 -> Some oid_varchar
    | 1016 -> Some oid_int8
    | 1021 -> Some oid_float4
    | 1022 -> Some oid_float8
    | 1028 -> Some oid_oid
    | 1115 -> Some oid_timestamp
    | 1182 -> Some oid_date
    | 1185 -> Some oid_timestamptz
    | 1187 -> Some oid_interval
    | 199 -> Some oid_json
    | 2951 -> Some oid_uuid
    | 3807 -> Some oid_jsonb
    | _ -> None

  (* An array's text form, one dimension, as Postgres writes it:
     [{1,"a b",NULL}], a quoted element's escapes
     undone and an unquoted NULL none. Each element is a cell in text, read
     as the element's scalar reads one. *)
  let elements cell =
    match cell with
    | Element _ | Undescribed -> Error "not an array"
    | Described (column, raw) -> (
        let element =
          {
            column with
            format = Pg.Column.Text;
            type_oid =
              Option.value ~default:column.type_oid
                (Option.bind (element_oid column.type_oid) Pg.Oid.of_int);
          }
        in
        let text_cell v = Element (element, v, column.type_oid) in
        let n = String.length raw in
        let rec quoted i b =
          if i >= n then Error "an unended quoted element"
          else
            match raw.[i] with
            | '"' -> Ok (Buffer.contents b, i + 1)
            | '\\' when i + 1 < n ->
                Buffer.add_char b raw.[i + 1];
                quoted (i + 2) b
            | c ->
                Buffer.add_char b c;
                quoted (i + 1) b
        in
        let rec plain i =
          match if i < n then Some raw.[i] else None with
          | Some (',' | '}') | None -> i
          | Some _ -> plain (i + 1)
        in
        let rec go i acc =
          let* element, j =
            if i < n && Char.equal raw.[i] '"' then
              Result.map
                (fun (v, j) -> (Some (text_cell v), j))
                (quoted (i + 1) (Buffer.create 16))
            else
              let j = plain i in
              let text = String.trim (String.sub raw i (j - i)) in
              if String.contains text '{' then
                Error "an array of more than one dimension"
              else if String.equal (String.uppercase_ascii text) "NULL" then
                Ok (None, j)
              else Ok (Some (text_cell text), j)
          in
          if j < n && Char.equal raw.[j] ',' then go (j + 1) (element :: acc)
          else if j = n - 1 && Char.equal raw.[j] '}' then
            Ok (List.rev (element :: acc))
          else Error "not an array"
        in
        (* Where the lower bound is not 1 Postgres writes the bounds first,
           [[0:1]={1,2}]; a list has no bounds to keep, so its elements are
           read in order, and one pair of bounds per dimension says how many
           there are. *)
        let braces =
          if n > 0 && Char.equal raw.[0] '[' then
            match String.index_opt raw '=' with
            | None -> Error "not an array"
            | Some e ->
                let dimensions =
                  String.fold_left
                    (fun k c -> if Char.equal c '[' then k + 1 else k)
                    0 (String.sub raw 0 e)
                in
                if dimensions = 1 then Ok (e + 1)
                else Error "an array of more than one dimension"
          else Ok 0
        in
        match column.format with
        | Pg.Column.Binary -> Error "an array in binary, which is read in text"
        | Pg.Column.Text ->
            let* start = braces in
            if String.equal raw "{}" then Ok []
            else if n - start >= 2 && Char.equal raw.[start] '{' then
              go (start + 1) []
            else Error "not an array")

  (* A transaction a failed statement aborted is not an error to COMMIT:
     the server rolls it back and says so in the command tag. *)
  let commit t =
    timed t "commit" @@ fun () ->
    Result.map
      (fun tag ->
        if String.equal (Pg.Tag.command tag) "ROLLBACK" then `Rolled_back
        else `Committed)
      (send t (fun pg -> Pg.execute pg "commit" ~params:[]))

  (* A constraint that refused the write because of other rows -- unique,
     exclusion, a foreign key either way round -- is the caller's answer and
     not a failure: a racing second writer is refused, and that refusal is
     what a caller reads as "read again". One about the row alone, NOT NULL
     or CHECK, is the program's mistake, and stays [`Db]. The constraint is
     named, because which one refused is the answer's meaning when a table
     has two. *)
  let other_rows = [ "23505"; "23P01"; "23503"; "23001" ]

  (* A statement sent and no answer back is [`Lost]: the driver timed out
     or the socket broke, or the server ended the session -- a FATAL or a
     PANIC, which it sends instead of the statement's answer. Whether the
     statement ran is not known. *)
  let error = function
    | Unsent -> `Closed
    | Failed (Pg.Server e as failure) when ends_session e ->
        `Lost (Pg.error_to_string failure)
    | Failed (Pg.Server e) when List.mem (Pg.Server_error.sqlstate e) other_rows
      ->
        `Conflict (Pg.Server_error.constraint_name e)
    (* 40001 is SQL's serialization failure and 40P01 Postgres's deadlock:
       the two a transaction run again may get past. *)
    | Failed (Pg.Server e as failure)
      when List.mem (Pg.Server_error.sqlstate e) [ "40001"; "40P01" ] ->
        `Not_serializable (Pg.error_to_string failure)
    | Failed ((Pg.Timeout | Pg.Closed | Pg.Io _ | Pg.Protocol _) as e) ->
        `Lost (Pg.error_to_string e)
    | Failed ((Pg.Server _ | Pg.Refused _) as e) -> db_error e
end

module Run = Rowtype.Make (Backend)
include (Run : Rowtype.S with type conn := conn)

(* Each row is bound as a statement's parameters are, as the driver reads
   the sequence, so a load is never held whole; a row that cannot be bound
   is raised out of the sequence, which fails the COPY with nothing written
   and the connection whole. *)
let copy_in t ?schema ~table ~columns row rows =
  let exception Unbound of Rowtype.error in
  let n = List.length columns in
  if n > 0 && n <> S.arity row then
    Error
      (`Db
         (Printf.sprintf "%d columns named, and the row has %d" n (S.arity row)))
  else
    let cells v =
      match Run.bind row v with
      | Ok params -> Array.of_list params
      | Error e -> raise (Unbound e)
    in
    (* The statement the driver sends, for the log and the observer. *)
    let quoted name =
      "\"" ^ String.concat "\"\"" (String.split_on_char '"' name) ^ "\""
    in
    let sql =
      Printf.sprintf "copy %s%s from stdin"
        (match schema with
        | Some s -> quoted s ^ "." ^ quoted table
        | None -> quoted table)
        (match columns with
        | [] -> ""
        | _ -> " (" ^ String.concat ", " (List.map quoted columns) ^ ")")
    in
    match
      timed t sql @@ fun () ->
      send t @@ fun pg ->
      Pg.copy_in_rows pg ?schema ~table ~columns (Seq.map cells rows)
    with
    | Ok tag -> Ok (Option.value (Pg.Tag.rows tag) ~default:0)
    | Error f -> Error (Backend.error f)
    | exception Unbound (#Rowtype.error as e) -> Error e

(* ------------------------------------------------------------------ *)
(* Connections *)

(* The driver reads a date or an instant in text only as [DateStyle=ISO]
   writes it, and an interval only as [IntervalStyle=postgres] does, and a
   connection that keeps no statements reads every cell in text, so every
   connection asks for both, over any style the caller named. *)
let with_output_styles given =
  ("DateStyle", "ISO")
  :: ("IntervalStyle", "postgres")
  :: List.filter
       (fun (name, _) ->
         match String.lowercase_ascii name with
         | "datestyle" | "intervalstyle" -> false
         | _ -> true)
       (Option.value given ~default:[])

let conninfo target =
  Result.map_error (fun m -> `Db m) (Conninfo.of_string target)

(* Through the connection string's own reading of itself, so a quoted value
   or a URL's query survives. *)
let on_database ~server name =
  Result.map
    (fun (c : Conninfo.t) -> Conninfo.to_url { c with database = name })
    (conninfo server)

let connect ~sw ~net ~mono_clock:clock ?parameters ?timeout_s ?statement_cache
    ?(observe = unobserved) c =
  Result.map
    (fun pg ->
      { pg; observe; clock :> Eio.Time.Mono.ty Eio.Std.r; ownership = Kept })
    (Result.map_error db_error
       (Pg.connect ~sw ~net ~clock
          ~parameters:(with_output_styles parameters)
          ?timeout_s ?statement_cache c))

let close t =
  t.ownership <- Closed_by_owner;
  Pg.close t.pg

(* A connection a failure closed -- the server restarted, a failover -- is
   made again in place, rather than left to fail every statement after it.
   Asked before a transaction, which is the only point where starting again
   loses nothing. *)
let revive t =
  match t.ownership with
  | Closed_by_owner -> Error `Closed
  | Kept ->
      if Pg.closed t.pg then Result.map_error db_error (Pg.reset t.pg)
      else Ok ()

let timeout_s t = Pg.timeout_s t.pg
let set_timeout_s t timeout_s = Pg.set_timeout t.pg ~timeout_s

module Pool = struct
  type t = {
    pool : Pg.Pool.t;
    observe : observer;
    clock : Eio.Time.Mono.ty Eio.Std.r;
  }

  let create ~sw ~net ~mono_clock:clock ?parameters ?timeout_s ?statement_cache
      ?size ?wait_s ?reset ?max_lifetime_s ?idle_check_s ?(observe = unobserved)
      c =
    Result.map
      (fun pool -> { pool; observe; clock :> Eio.Time.Mono.ty Eio.Std.r })
      (Result.map_error db_error
         (Pg.Pool.create ~sw ~net ~clock
            ~parameters:(with_output_styles parameters)
            ?timeout_s ?statement_cache ?size ?wait_s ?reset ?max_lifetime_s
            ?idle_check_s c))

  let use ?wait_s t f =
    Pg.Pool.use ?wait_s t.pool (fun pg ->
        f { pg; observe = t.observe; clock = t.clock; ownership = Kept })

  type stats = Pg.Pool.stats = {
    size : int;
    idle : int;
    waiting : int;
    replaced : int;
  }

  let stats t = Pg.Pool.stats t.pool
  let close t = Pg.Pool.close t.pool
end

(* ------------------------------------------------------------------ *)
(* Notifications *)

(* The driver's listener, its failures told as a statement's are, so what
   listens through this adapter names no driver. *)
module Listener = struct
  type t = Pg.Listener.t

  type event =
    | Notification of { channel : string; payload : string }
    | Reconnected

  let connect ~sw ~net ~mono_clock:clock ?parameters ?timeout_s ?heartbeat_s c =
    Result.map_error db_error
      (Pg.Listener.connect ~sw ~net ~clock ?parameters ?timeout_s ?heartbeat_s c)

  (* A listener closed hears nothing more, and sends nothing. *)
  let error = function Pg.Closed -> `Closed | e -> db_error e
  let listen t channel = Result.map_error error (Pg.Listener.listen t channel)

  let unlisten t channel =
    Result.map_error error (Pg.Listener.unlisten t channel)

  let next t =
    match Pg.Listener.next t with
    | Ok (Pg.Listener.Notification n) ->
        Ok (Notification { channel = n.channel; payload = n.payload })
    | Ok Pg.Listener.Reconnected -> Ok Reconnected
    | Error e -> Error (error e)

  let close = Pg.Listener.close
end

(* ------------------------------------------------------------------ *)
(* Transactions *)

module Transaction = struct
  type isolation = Read_committed | Repeatable_read | Serializable

  type failure =
    [ `Not_committed of string | `Not_serializable of string | `Lost of string ]

  (* No level named is the database's own default, so a plain [begin]. *)
  let begin_statement = function
    | None -> "begin"
    | Some Read_committed -> "begin isolation level read committed"
    | Some Repeatable_read -> "begin isolation level repeatable read"
    | Some Serializable -> "begin isolation level serializable"

  let not_committed what detail =
    Log.err (fun m -> m "a transaction was not committed: %s: %s" what detail);
    Error (`Not_committed (what ^ ": " ^ detail))

  (* A connection the server dropped does not know it until it next speaks,
     and [begin] is the one statement where starting again loses nothing. *)
  let begin_ db statement =
    match exec_raw db statement with
    | Ok () -> Ok ()
    | Error _ ->
        let* () = revive db in
        exec_raw db statement

  let rollback db = ignore (exec_raw db "rollback" : (unit, S.error) result)

  (* A COMMIT sent and no answer back may have committed: the server can
     commit and lose the connection before its answer is read. *)
  let lost detail =
    Log.err (fun m -> m "a transaction's COMMIT was lost: %s" detail);
    Error (`Lost detail)

  (* Postgres answers COMMIT on an aborted transaction with a rollback and no
     error, so what the work answered is what tells the two apart: a refusal
     that stopped at the failed statement is the work's answer, as a returned
     conflict is; an [Ok] means somebody swallowed the failure and would have
     answered success for work that is not there. *)
  let once ~keep ~isolation db work =
    match
      let* () = revive db in
      begin_ db (begin_statement isolation)
    with
    | Error e -> not_committed "begin" (S.error_to_string e)
    | Ok () -> (
        match work db with
        (* A cancelled fiber can send nothing, a rollback included: the
           driver closes the connection, which ends the transaction. *)
        | exception (Eio.Cancel.Cancelled _ as ex) -> raise ex
        | exception ex ->
            let trace = Printexc.get_raw_backtrace () in
            rollback db;
            Printexc.raise_with_backtrace ex trace
        | Ok v -> (
            match commit db with
            | Ok `Committed -> Ok v
            | Ok `Rolled_back ->
                not_committed "commit"
                  "the work answered Ok in a transaction a failed statement \
                   had aborted"
            | Error (`Not_serializable detail) ->
                Error (`Not_serializable detail)
            | Error (`Lost detail) -> lost detail
            | Error e -> not_committed "commit" (S.error_to_string e))
        | Error e when not (keep e) ->
            rollback db;
            Error e
        | Error e -> (
            match commit db with
            | Ok `Committed -> Error e
            (* The answer stands; that the write it asked to keep did not is
               the log's to say, or an attempt counted counts nothing. *)
            | Ok `Rolled_back ->
                Log.warn (fun m ->
                    m
                      "a refusal that kept its write was rolled back: a \
                       statement in its transaction had failed");
                Error e
            | Error (`Not_serializable detail) ->
                Error (`Not_serializable detail)
            | Error (`Lost detail) -> lost detail
            | Error failure ->
                not_committed "commit" (S.error_to_string failure)))

  (* The whole transaction again, from [begin] and at once, which is what
     Postgres asks of a serialization failure; the bound is what keeps
     contention that does not clear from spinning.

     A transaction inside another on one connection is refused before it
     begins: Postgres only warns at a second [begin], and the inner [commit]
     would end the outer transaction with the outer work half done. The
     status is read after [revive], since a connection lost inside a
     transaction last heard that it was in one. *)
  let within ?(keep = fun _ -> false) ?isolation ?(retries = 0) db work =
    let rec attempt left =
      match once ~keep ~isolation db work with
      | Error (`Not_serializable detail) when left > 0 ->
          Log.info (fun m ->
              m "a transaction was not serializable, run again: %s" detail);
          attempt (left - 1)
      | answer -> answer
    in
    match revive db with
    | Error e -> not_committed "begin" (S.error_to_string e)
    | Ok () -> (
        match Pg.status db.pg with
        | Pg.Protocol.Idle -> attempt retries
        | Pg.Protocol.In_transaction | Pg.Protocol.Failed ->
            not_committed "begin"
              "a transaction is already open on this connection")
end

(* ------------------------------------------------------------------ *)
(* Statements against the database *)

(* A type as the check compares it: an enum by name, since a label is read
   as text, and anything else by the oid of what it is -- a domain by the
   type it is made from. *)
type resolved = Enum of string | Base of int * string | Array of int * string

let pg_type =
  S.find_opt ~params:S.int
    ~row:(S.t5 S.text S.text S.int S.text S.int)
    "select typname::text, typtype::text, typbasetype::int8, \
     typcategory::text, typelem::int8 from pg_type where oid = $1"

(* [None] for an oid pg_type does not have; [Error] where it could not be
   asked, which is the check's failure and not the statement's. *)
let rec resolve db oid =
  match run db pg_type oid with
  | Ok (Some (name, "e", _, _, _)) -> Ok (Some (Enum name))
  | Ok (Some (_, "d", base, _, _)) -> resolve db base
  | Ok (Some (name, _, _, "A", element)) -> Ok (Some (Array (element, name)))
  | Ok (Some (name, _, _, _, _)) -> Ok (Some (Base (oid, name)))
  | Ok None -> Ok None
  | Error e -> Error e

let scalar_name : type a. a S.scalar -> string = function
  | S.Int -> "int"
  | S.Int64 -> "int64"
  | S.Float -> "float"
  | S.Text -> "text"
  | S.Bytes -> "bytes"
  | S.Bool -> "bool"
  | S.Instant -> "instant"
  | S.Date -> "date"
  | S.Timestamp -> "timestamp"
  | S.Interval -> "interval"
  | S.Uuid -> "uuid"
  | S.Json -> "json"

(* A parameter is written and a column read: an enum takes a label bound
   as text, and is read only through a cast, as {!Backend.read_as} says. *)
type use = Written | Read

(* What each scalar reads and writes: Postgres's own types as a read
   checks them, and an enum's label written as text. *)
let fits : type a. use -> a S.scalar -> resolved -> bool =
 fun use scalar t ->
  match (scalar, t) with
  | S.Text, Enum _ -> ( match use with Written -> true | Read -> false)
  | ( ( S.Int | S.Int64 | S.Float | S.Bytes | S.Bool | S.Instant | S.Date
      | S.Timestamp | S.Interval | S.Uuid | S.Json ),
      Enum _ ) ->
      false
  | ( ( S.Int | S.Int64 | S.Float | S.Text | S.Bytes | S.Bool | S.Instant
      | S.Date | S.Timestamp | S.Interval | S.Uuid | S.Json ),
      Array _ ) ->
      false
  | ( ( S.Int | S.Int64 | S.Float | S.Text | S.Bytes | S.Bool | S.Instant
      | S.Date | S.Timestamp | S.Interval | S.Uuid | S.Json ),
      Base (oid, _) ) ->
      reads_builtin scalar oid

(* What is wrong with one declared column, [None] where nothing is. *)
let mismatch db use i column oid =
  let what = match use with Written -> "parameter" | Read -> "column" in
  if oid = 0 || oid = oid_unknown then Ok None
  else
    let* t = resolve db oid in
    match (column, t) with
    | S.Column s, Some t when fits use s t -> Ok None
    | S.Array_of [ S.Column s ], Some (Array (element, name)) -> (
        let* t = resolve db element in
        match t with
        | Some t when fits use s t -> Ok None
        | Some _ | None ->
            Ok
              (Some
                 (Printf.sprintf
                    "%s %d is %s, which an array of %s does not read%s" what i
                    name (scalar_name s)
                    (cast_hint s element "::text[]"))))
    | S.Array_of _, Some (Array _) ->
        Ok
          (Some
             (Printf.sprintf
                "%s %d: an array's element is one column and no array of its \
                 own"
                what i))
    | S.Column s, Some (Enum name | Base (_, name) | Array (_, name)) ->
        Ok
          (Some
             (Printf.sprintf "%s %d is %s, which %s does not read%s" what i name
                (scalar_name s) (cast_hint s oid "::text")))
    | S.Array_of _, Some (Enum name | Base (_, name)) ->
        Ok
          (Some
             (Printf.sprintf "%s %d is %s, which an array does not read" what i
                name))
    | (S.Column _ | S.Array_of _), None ->
        Ok
          (Some (Printf.sprintf "%s %d is a type the check cannot find" what i))

(* Declared columns against the database's, in order and in number. *)
let mismatches db use declared actual =
  let counted = match use with Written -> "parameters" | Read -> "columns" in
  let declared_n = List.length declared and actual_n = List.length actual in
  let rec go i declared actual acc =
    match (declared, actual) with
    | [], [] -> Ok (List.rev acc)
    | column :: declared_rest, oid :: actual_rest ->
        let* problem = mismatch db use i column oid in
        go (i + 1) declared_rest actual_rest
          (Option.fold ~none:acc ~some:(fun p -> p :: acc) problem)
    | _, _ ->
        Ok
          (List.rev
             (Printf.sprintf "%d %s declared, and the database has %d"
                declared_n counted actual_n
             :: acc))
  in
  go 1 declared actual []

(* One statement's problems; [Error] where the check could not ask: a
   server's refusal is the statement's, any other failure the connection's. *)
let problems db (declared : S.declared) =
  match send db (fun pg -> Pg.describe pg declared.sql) with
  | Error (Failed (Pg.Server e as failure)) when not (ends_session e) ->
      Ok [ "the database refuses it: " ^ Pg.error_to_string failure ]
  | Error f -> Error (Backend.error f)
  | Ok d ->
      let* parameters =
        mismatches db Written declared.parameters
          (List.map Pg.Oid.to_int d.parameters)
      in
      let* columns =
        match declared.row with
        | None -> Ok []
        | Some row ->
            mismatches db Read row
              (Array.to_list
                 (Array.map
                    (fun (c : Pg.Column.t) -> Pg.Oid.to_int c.type_oid)
                    d.columns))
      in
      Ok (parameters @ columns)

let verify db statements =
  let rec each found = function
    | [] ->
        if List.is_empty found then Ok ()
        else Error (`Disagreements (List.rev found))
    | statement :: rest -> (
        let declared = S.declared statement in
        match problems db declared with
        | Error e -> Error e
        | Ok statement_problems ->
            each
              (List.rev_append
                 (List.map
                    (fun p -> first_line declared.sql ^ ": " ^ p)
                    statement_problems)
                 found)
              rest)
  in
  each [] statements

(* rowtype over SQLite, through the sqlite3 binding: every scalar kept as
   one of SQLite's four kinds of value, and every call that can wait made
   on a system thread, since SQLite waits on the disk and on another
   connection's lock, and Eio's domain would wait with it. *)

module S = Rowtype
module D = Sqlite3.Data
module Rc = Sqlite3.Rc

let ( let* ) = Result.bind

type observer = { around : 'a. string -> (unit -> 'a) -> 'a }

let unobserved = { around = (fun _ f -> f ()) }

(* A connection, what watches the statements it runs, and the monotonic
   clock a statement is timed on, which a wall clock that jumps cannot
   move. *)
type conn = {
  db : Sqlite3.db;
  observe : observer;
  clock : Eio.Time.Mono.ty Eio.Std.r;
  mutable closed : bool;
}

let src =
  Logs.Src.create "rowtype-sqlite" ~doc:"Each statement, and how long it took"

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

(* Eio waits for the thread whatever happens to the fiber, so a statement
   is never in two threads' hands, and a fiber already cancelled never
   starts one. *)
let off f = Eio_unix.run_in_systhread ~label:"rowtype-sqlite" f

(* What SQLite answered instead of a result, read before another call on
   the connection replaces it; or what was refused before SQLite was
   asked. *)
type failure =
  | Sqlite of { code : Rc.t; extended : int; message : string }
  | Closed
  | Refused of string

(* SQLite's words where it has not run anything, which quote only the
   statement's text: [no such table: t], [near "$1": syntax error]. *)
let failed db code =
  Sqlite
    {
      code;
      extended = Sqlite3.extended_errcode_int db;
      message = Sqlite3.errmsg db;
    }

let sqlite_constraint_check = 275
let sqlite_constraint_notnull = 1299
let sqlite_constraint_unique = 2067
let sqlite_constraint_primarykey = 1555
let sqlite_constraint_foreignkey = 787

(* Running a statement, a function quotes its argument in its message, each
   function its own way, and a trigger's RAISE says what its author wrote,
   so the words are the code's, as Postgres's class 22 is told; the
   constraints SQLite checks itself name the schema's columns alone. *)
let stepped db code =
  let extended = Sqlite3.extended_errcode_int db in
  let message =
    if
      List.mem extended
        [
          sqlite_constraint_check;
          sqlite_constraint_notnull;
          sqlite_constraint_unique;
          sqlite_constraint_primarykey;
        ]
    then Sqlite3.errmsg db
    else
      Printf.sprintf
        "SQLite answered %s (extended code %d) running the statement (its \
         words are left out, since they may quote a value)"
        (Rc.to_string code) extended
  in
  Sqlite { code; extended; message }

(* The binding raises where it was handed a statement it no longer has;
   everything else answers a code. *)
let guarded db f =
  match f () with
  | v -> v
  | exception Sqlite3.SqliteError _ -> Error (failed db (Sqlite3.errcode db))
  | exception Sqlite3.Error m -> Error (Refused m)

let exec db sql =
  match Sqlite3.exec db sql with Rc.OK -> Ok () | rc -> Error (failed db rc)

let finalize stmt =
  match Sqlite3.finalize stmt with
  | (_ : Rc.t) -> ()
  | exception Sqlite3.SqliteError _ -> ()

(* The binding raises where SQLite refused the text, and where it compiled
   nothing from it -- a space, a comment, a lone [;] -- which SQLite answers
   OK; the connection's code tells the two apart. *)
let compiled db f =
  match f () with
  | stmt -> Ok stmt
  | exception Sqlite3.Error _ -> (
      match Sqlite3.errcode db with
      | Rc.OK -> Ok None
      | code -> Error (failed db code))

(* One statement: SQLite prepares the first of a text and leaves the rest,
   which would never run. *)
let prepare db sql =
  match compiled db (fun () -> Some (Sqlite3.prepare db sql)) with
  | Error e -> Error e
  | Ok None -> Error (Refused "a text with no statement")
  | Ok (Some stmt) -> (
      match compiled db (fun () -> Sqlite3.prepare_tail stmt) with
      | Ok None -> Ok stmt
      | Ok (Some tail) ->
          finalize tail;
          finalize stmt;
          Error
            (Refused "a text of several statements, which only exec_raw runs")
      | Error e ->
          finalize stmt;
          Error e)

let values n = if n = 1 then "1 value" else Printf.sprintf "%d values" n

(* The value each placeholder takes, by its number: [?] numbered as SQLite
   numbers it, [?3] and [$3] by theirs. A placeholder with a name has no
   value in a shape, and a value no placeholder takes is a statement and a
   shape that disagree, as Postgres says of one. [run] binds by it and
   [verify] checks by it, so neither passes what the other refuses. *)
let numbers placeholders n =
  let taken = Array.make n false in
  let number i = function
    | None -> Ok i
    | Some name ->
        (* Its first character is what marks it: [?], [$], [:] or [@]. *)
        let digits =
          if String.length name > 1 then
            String.sub name 1 (String.length name - 1)
          else ""
        in
        if
          String.length digits > 0
          && String.for_all (function '0' .. '9' -> true | _ -> false) digits
        then
          Option.to_result
            ~none:("the placeholder " ^ name ^ " is past any value")
            (int_of_string_opt digits)
        else
          Error
            ("the placeholder " ^ name
           ^ " has a name, and a shape's values have none: number it, as $1")
  in
  let rec each i acc = function
    | [] -> Ok (List.rev acc)
    | name :: rest ->
        let* k = number i name in
        if k < 1 || k > n then
          Error
            (Printf.sprintf "the placeholder %s is past the %s given"
               (Option.value name ~default:"?")
               (values n))
        else (
          taken.(k - 1) <- true;
          each (i + 1) (k :: acc) rest)
  in
  let* ks = each 1 [] placeholders in
  let rec untaken k =
    if k = n then Ok ks
    else if taken.(k) then untaken (k + 1)
    else
      Error
        (Printf.sprintf "%s given, and no placeholder takes value %d" (values n)
           (k + 1))
  in
  untaken 0

let bind db stmt params =
  let given = Array.of_list params in
  let placeholders =
    List.init (Sqlite3.bind_parameter_count stmt) (fun i ->
        Sqlite3.bind_parameter_name stmt (i + 1))
  in
  match numbers placeholders (Array.length given) with
  | Error m -> Error (Refused m)
  | Ok ks ->
      let rec each i = function
        | [] -> Ok ()
        | k :: rest -> (
            match Sqlite3.bind stmt i given.(k - 1) with
            | Rc.OK -> each (i + 1) rest
            | rc -> Error (failed db rc))
      in
      each 1 ks

(* Rows read on the system thread a hop, measured on an Apple M-series
   machine: a hop costs about 6.5 us, fifty times a step, and 64 rows a
   hop read at half the speed of reading them in place, holding no more
   than 64 at a time. *)
let rows_per_hop = 64

(* Each run of a prepared statement, with its parameters, to its end, its
   rows discarded. *)
let rec to_end db stmt =
  match Sqlite3.step stmt with
  | Rc.ROW -> to_end db stmt
  | Rc.DONE -> Ok ()
  | rc -> Error (stepped db rc)

let run_each db stmt runs =
  List.fold_left
    (fun so_far params ->
      let* () = so_far in
      let* () = bind db stmt params in
      let* () = to_end db stmt in
      ignore (Sqlite3.reset stmt : Rc.t);
      Ok ())
    (Ok ()) runs

(* Each statement of a text, prepared only once the one before it has run,
   since that one may make what the next names. *)
let each_statement db sql =
  let rec from stmt =
    let next =
      Fun.protect
        ~finally:(fun () -> finalize stmt)
        (fun () ->
          let* () = to_end db stmt in
          compiled db (fun () -> Sqlite3.prepare_tail stmt))
    in
    match next with
    | Ok (Some stmt) -> from stmt
    | Ok None -> Ok ()
    | Error e -> Error e
  in
  match compiled db (fun () -> Some (Sqlite3.prepare db sql)) with
  | Ok (Some stmt) -> from stmt
  | Ok None -> Ok ()
  | Error e -> Error e

(* Work SQLite undoes whole goes in a savepoint: inside a transaction, the
   transaction's own writes stay, and outside one the savepoint is a
   transaction of its own. *)
let undo db savepoint =
  ignore (exec db ("rollback to " ^ savepoint) : (unit, failure) result);
  ignore (exec db ("release " ^ savepoint) : (unit, failure) result)

(* Released outside a transaction, a savepoint commits, which a deferred
   foreign key or another's lock can refuse, and leaves it open. *)
let release db savepoint =
  match exec db ("release " ^ savepoint) with
  | Ok () -> Ok ()
  | Error e ->
      undo db savepoint;
      Error e

type cell = Column of D.t | Element of D.t

let row stmt =
  Array.map
    (function D.NULL | D.NONE -> None | v -> Some (Column v))
    (Sqlite3.row_data stmt)

type chunk =
  | More of cell option array list
  | Done of cell option array list * int

(* The next rows, in order, and at the end how many rows the statement
   changed or sent: one that answers rows -- a select, a write with
   [returning] -- counts them, as Postgres does, and any other counts the
   rows SQLite says it changed. *)
let chunk db stmt =
  let rec go k acc =
    if k = rows_per_hop then Ok (More (List.rev acc))
    else
      match Sqlite3.step stmt with
      | Rc.ROW -> go (k + 1) (row stmt :: acc)
      | Rc.DONE -> Ok (Done (List.rev acc, Sqlite3.changes db))
      | rc -> Error (stepped db rc)
  in
  go 0 []

let kind = function
  | D.INT _ -> "INTEGER"
  | D.FLOAT _ -> "REAL"
  | D.TEXT _ -> "TEXT"
  | D.BLOB _ -> "BLOB"
  | D.NULL | D.NONE -> "NULL"

(* Where a float holds every integer exactly: 2^53. *)
let exact_float = 9_007_199_254_740_992L

module Backend = struct
  type nonrec conn = conn
  type param = D.t
  type nonrec cell = cell
  type nonrec failure = failure

  let null = D.NULL

  let param : type a. a S.scalar -> a -> D.t =
   fun s v ->
    match s with
    | S.Int -> D.INT (Int64.of_int v)
    | S.Int64 -> D.INT v
    | S.Float -> D.FLOAT v
    | S.Text -> D.TEXT v
    | S.Bytes -> D.BLOB v
    | S.Bool -> D.INT (if v then 1L else 0L)
    | S.Instant -> D.TEXT (Forms.instant v)
    | S.Date -> D.TEXT (Forms.date v)
    | S.Timestamp -> D.TEXT (Forms.instant v)
    | S.Interval -> D.TEXT (Forms.interval v)
    | S.Uuid -> D.TEXT (Uuidm.to_string v)
    | S.Json -> D.TEXT v
    | S.Enum _ -> D.TEXT v

  (* A refusal names the kind of value that came, and never the value,
     which may be a token or an address and would reach a log. *)
  let read : type a. a S.scalar -> cell -> (a, string) result =
   fun s cell ->
    let value = match cell with Column v | Element v -> v in
    let expected what = Error ("expected " ^ what ^ ", got " ^ kind value) in
    let text what parse =
      match value with
      | D.TEXT t -> (
          match parse t with
          | Some v -> Ok v
          | None -> Error ("expected " ^ what ^ ", got text that is not one"))
      | _ -> expected "TEXT"
    in
    match (s, value) with
    | S.Int, D.INT i ->
        let n = Int64.to_int i in
        if Int64.equal (Int64.of_int n) i then Ok n
        else
          Error
            "an integer past 63 bits, which an int cannot hold: read it with \
             Rowtype.int64"
    | S.Int, _ -> expected "INTEGER"
    | S.Int64, D.INT i -> Ok i
    | S.Int64, _ -> expected "INTEGER"
    | S.Float, D.FLOAT f -> Ok f
    | S.Float, D.INT i ->
        if
          Int64.compare i exact_float <= 0
          && Int64.compare i (Int64.neg exact_float) >= 0
        then Ok (Int64.to_float i)
        else
          Error
            "an integer past 53 bits, which a float cannot hold exactly: read \
             it with Rowtype.int64"
    | S.Float, _ -> expected "REAL"
    | S.Text, D.TEXT t -> Ok t
    | S.Text, _ -> expected "TEXT"
    | S.Bytes, D.BLOB b -> (
        match cell with Column _ -> Ok b | Element _ -> expected "hex TEXT")
    | S.Bytes, D.TEXT _ -> (
        match cell with
        | Element _ -> text "hex" Forms.of_hex
        | Column _ -> expected "BLOB")
    | S.Bytes, _ -> expected "BLOB"
    | S.Bool, D.INT 0L -> Ok false
    | S.Bool, D.INT 1L -> Ok true
    | S.Bool, D.INT _ -> Error "expected 0 or 1, got another integer"
    | S.Bool, _ -> expected "INTEGER"
    | S.Instant, _ ->
        text "an instant" (fun t -> Option.map fst (Forms.date_time t))
    | S.Timestamp, D.TEXT t -> (
        match Forms.date_time t with
        | Some (v, None) -> Ok v
        | Some (_, Some _) ->
            Error
              "a time with a zone, which a timestamp does not hold: read it \
               with Rowtype.instant"
        | None -> Error "expected a timestamp, got text that is not one")
    | S.Timestamp, _ -> expected "TEXT"
    | S.Date, _ -> text "a date" Forms.date_of_string
    | S.Interval, _ -> text "an interval" Forms.interval_of_string
    | S.Uuid, D.BLOB b when String.length b = 16 -> (
        match Uuidm.of_binary_string b with
        | Some u -> Ok u
        | None -> expected "a uuid")
    | S.Uuid, _ ->
        (* [Uuidm.of_string] reads a uuid at the start and ignores what
           follows it. *)
        text "a uuid" (fun t ->
            if String.length t = 36 then Uuidm.of_string t else None)
    | S.Json, D.TEXT t -> Ok t
    | S.Json, _ -> expected "TEXT"
    | S.Enum _, D.TEXT t -> Ok t
    | S.Enum _, _ -> expected "TEXT"

  let fold t sql params ~init ~row =
    if t.closed then Error Closed
    else
      timed t sql @@ fun () ->
      let prepared =
        off (fun () ->
            guarded t.db (fun () ->
                let* stmt = prepare t.db sql in
                match bind t.db stmt params with
                | Ok () -> Ok (stmt, Sqlite3.column_count stmt)
                | Error e ->
                    finalize stmt;
                    Error e))
      in
      match prepared with
      | Error e -> Error e
      | Ok (stmt, columns) ->
          (* Ending a write left part-read commits it, which can wait on a
             lock, and a cancelled fiber may not start the hop. *)
          Fun.protect ~finally:(fun () ->
              Eio.Cancel.protect (fun () -> off (fun () -> finalize stmt)))
          @@ fun () ->
          let rec go acc sent =
            match off (fun () -> guarded t.db (fun () -> chunk t.db stmt)) with
            | Error e -> Error e
            | Ok (More rows) ->
                go (List.fold_left row acc rows) (sent + List.length rows)
            | Ok (Done (rows, changed)) ->
                let sent = sent + List.length rows in
                Ok
                  ( List.fold_left row acc rows,
                    if columns > 0 then sent else changed )
          in
          go init 0

  let batch t sql runs =
    if t.closed then Error Closed
    else
      timed t sql @@ fun () ->
      off @@ fun () ->
      let* () = exec t.db "savepoint rowtype_batch" in
      match
        guarded t.db (fun () ->
            let* stmt = prepare t.db sql in
            Fun.protect
              ~finally:(fun () -> finalize stmt)
              (fun () -> run_each t.db stmt runs))
      with
      | Error e ->
          undo t.db "rowtype_batch";
          Error e
      | Ok () -> release t.db "rowtype_batch"

  let script t sql =
    if t.closed then Error Closed
    else
      timed t sql @@ fun () ->
      off (fun () -> guarded t.db (fun () -> each_statement t.db sql))

  let array elements = D.TEXT (Forms.array elements)

  let elements = function
    | Column (D.TEXT t) -> (
        match Forms.elements t with
        | Ok values -> Ok (List.map (Option.map (fun v -> Element v)) values)
        | Error `Nested ->
            Error "an array of more than one dimension, or of JSON objects"
        | Error `Not_an_array ->
            Error "expected a JSON array, got text that is not an array")
    | Column v -> Error ("expected a JSON array as TEXT, got " ^ kind v)
    | Element _ -> Error "an array of more than one dimension"

  (* A unique key, a primary key and a foreign key refuse a write because
     of other rows, which is the caller's answer to give; SQLite names the
     columns of the first two, [UNIQUE constraint failed: t.a, t.b], and
     nothing of the third. NOT NULL and CHECK refuse the row alone, the
     program's mistake. *)
  let error : failure -> [> S.error ] = function
    | Closed -> `Connection `Closed
    | Refused m -> `Db m
    | Sqlite { code = Rc.CONSTRAINT; extended; message }
      when extended = sqlite_constraint_unique
           || extended = sqlite_constraint_primarykey -> (
        match String.index_opt message ':' with
        | Some i ->
            `Conflict
              (Some
                 (String.trim
                    (String.sub message (i + 1) (String.length message - i - 1))))
        | None -> `Conflict None)
    | Sqlite { code = Rc.CONSTRAINT; extended; _ }
      when extended = sqlite_constraint_foreignkey ->
        `Conflict None
    (* A lock another connection still held when the wait ran out: nothing
       of the statement was done, and running it again may succeed. *)
    | Sqlite { code = Rc.BUSY | Rc.LOCKED; message; _ } ->
        `Not_serializable message
    | Sqlite { message; _ } -> `Db message
end

module Run = Rowtype.Make (Backend)
include (Run : Rowtype.S with type conn := conn)

(* ------------------------------------------------------------------ *)
(* Connections *)

(* Closing a WAL file's last connection checkpoints it. *)
let close t =
  if not t.closed then (
    t.closed <- true;
    Eio.Cancel.protect (fun () ->
        off (fun () -> ignore (Sqlite3.db_close t.db : bool))))

(* ROLLBACK answers SQLITE_ERROR where no transaction is open -- SQLite
   rolled it back itself, or none was begun -- and any other failure may
   leave one open. *)
let rolled_back = function
  | Ok () | Error (Sqlite { code = Rc.ERROR; _ } | Closed) -> Ok ()
  | Error ((Sqlite _ | Refused _) as e) -> Error e

(* SQLite's wait is whole milliseconds, a C int of them. *)
let milliseconds wait =
  let most = Mtime.Span.(0x7fff_ffff * ms) in
  if Mtime.Span.compare wait most >= 0 then 0x7fff_ffff
  else
    Int64.to_int
      (Int64.div (Int64.add (Mtime.Span.to_uint64_ns wait) 999_999L) 1_000_000L)

let connect ~sw ~mono_clock ?(timeout = Mtime.Span.(5 * s))
    ?(observe = unobserved) file =
  let opened =
    off (fun () ->
        match Sqlite3.db_open file with
        | exception Sqlite3.SqliteError m -> Error m
        | exception Sqlite3.Error m -> Error m
        | db -> (
            Sqlite3.busy_timeout db (milliseconds timeout);
            (* SQLite leaves foreign keys unenforced unless each connection
               asks, which a schema that declares one never means. *)
            match exec db "pragma foreign_keys = on" with
            | Ok () -> Ok db
            | Error _ ->
                let m = Sqlite3.errmsg db in
                ignore (Sqlite3.db_close db : bool);
                Error m))
  in
  match opened with
  | Error m -> Error (`Unsent ("the database could not be opened: " ^ m))
  | Ok db ->
      let t =
        {
          db;
          observe;
          clock = (mono_clock :> Eio.Time.Mono.ty Eio.Std.r);
          closed = false;
        }
      in
      Eio.Switch.on_release sw (fun () -> close t);
      Ok t

(* A name inside a statement, which cannot be a parameter: quoted, its
   quotes doubled, it is any name the table has. *)
let quoted name =
  "\"" ^ String.concat "\"\"" (String.split_on_char '"' name) ^ "\""

(* The rows bound in the fiber, where the sequence may wait on anything,
   and written a chunk a hop, as a fold reads them, inside a savepoint that
   is released at the end and undone at any failure or raise -- the
   finalising and the undoing shielded, since a cancelled fiber may start
   no statement. *)
let copy_in t ?schema ~table ~columns row rows =
  let n = List.length columns and arity = S.arity row in
  if n > 0 && n <> arity then
    Error (`Db (Printf.sprintf "%d columns named, and the row has %d" n arity))
  else if t.closed then Error (`Connection `Closed)
  else
    let sql =
      Printf.sprintf "insert into %s%s values (%s)"
        (match schema with
        | Some s -> quoted s ^ "." ^ quoted table
        | None -> quoted table)
        (match columns with
        | [] -> ""
        | _ -> " (" ^ String.concat ", " (List.map quoted columns) ^ ")")
        (String.concat ", "
           (List.init arity (fun i -> "?" ^ string_of_int (i + 1))))
    in
    let savepoint = "rowtype_load" in
    timed t sql @@ fun () ->
    match
      off (fun () ->
          guarded t.db (fun () ->
              let* () = exec t.db ("savepoint " ^ savepoint) in
              match prepare t.db sql with
              | Ok stmt -> Ok stmt
              | Error e ->
                  undo t.db savepoint;
                  Error e))
    with
    | Error f -> Error (Backend.error f)
    | Ok stmt -> (
        let rec take rows k acc =
          if k = rows_per_hop then Ok (List.rev acc, Some rows)
          else
            match rows () with
            | Seq.Nil -> Ok (List.rev acc, None)
            | Seq.Cons (r, rest) ->
                let* params = Run.bind row r in
                take rest (k + 1) (params :: acc)
        in
        let rec load rows written =
          let* chunk, rest = take rows 0 [] in
          let* () =
            Result.map_error Backend.error
              (off (fun () -> guarded t.db (fun () -> run_each t.db stmt chunk)))
          in
          let written = written + List.length chunk in
          match rest with Some rest -> load rest written | None -> Ok written
        in
        let answer =
          match load rows 0 with
          | v -> Ok v
          | exception ex -> Error (ex, Printexc.get_raw_backtrace ())
        in
        let ended =
          Eio.Cancel.protect (fun () ->
              off (fun () ->
                  finalize stmt;
                  match answer with
                  | Ok (Ok _) -> release t.db savepoint
                  | Ok (Error _) | Error _ ->
                      undo t.db savepoint;
                      Ok ()))
        in
        match (answer, ended) with
        | Error (ex, trace), _ -> Printexc.raise_with_backtrace ex trace
        | Ok (Error e), _ -> Error e
        | Ok (Ok written), Ok () -> Ok written
        | Ok (Ok _), Error f -> Error (Backend.error f))

module Pool = struct
  type t = {
    free : conn Eio.Stream.t;
    size : int;
    waiting : int Atomic.t;
    closed : unit Eio.Promise.t;
    close_now : unit Eio.Promise.u;
    clock : Eio.Time.Mono.ty Eio.Std.r;
    wait : Mtime.Span.t;
    remake : unit -> (conn, S.error) result;
  }

  (* Each connection to a database in memory has one of its own, so a pool
     of them would be as many databases as connections. *)
  let in_memory file = String.equal file ":memory:" || String.equal file ""

  let create ~sw ~mono_clock ?timeout ?(size = 8) ?(wait = Mtime.Span.(5 * s))
      ?observe file =
    if in_memory file then
      Error
        (`Db
           "a database in memory is each connection's own, so a pool of \
            them             is many databases: give the pool a file")
    else
      let size = max 1 size in
      let remake () = connect ~sw ~mono_clock ?timeout ?observe file in
      let rec made k acc =
        if k = 0 then Ok acc
        else
          match remake () with
          | Ok c -> made (k - 1) (c :: acc)
          | Error e ->
              List.iter close acc;
              Error e
      in
      let* all = made size [] in
      let free = Eio.Stream.create size in
      List.iter (Eio.Stream.add free) all;
      let closed, close_now = Eio.Promise.create () in
      Ok
        {
          free;
          size;
          waiting = Atomic.make 0;
          closed;
          close_now;
          clock = (mono_clock :> Eio.Time.Mono.ty Eio.Std.r);
          wait;
          remake = (fun () -> (remake () :> (conn, S.error) result));
        }

  type error = [ `Pool of [ `Busy | `Closed ] ]

  (* Several branches can finish at once, and a loser's answer is dropped;
     were it a connection, the pool would lose it. So the connection wins. *)
  let take t ~wait =
    Atomic.incr t.waiting;
    Fun.protect
      ~finally:(fun () -> Atomic.decr t.waiting)
      (fun () ->
        Eio.Fiber.any
          ~combine:(fun a b -> match a with Ok _ -> a | Error _ -> b)
          [
            (fun () -> Ok (Eio.Stream.take t.free));
            (fun () ->
              Eio.Promise.await t.closed;
              Error `Closed);
            (fun () ->
              Eio.Time.Mono.sleep_span t.clock wait;
              Error `Busy);
          ])

  (* A connection goes back with no transaction open, since SQLite keeps one
     a borrower left open for whoever borrows next; one its borrower closed,
     or whose rollback failed, is made again, and kept closed, its
     statements saying so, where it cannot be. Once the pool is closed, it
     is closed. *)
  let give_back t c =
    if Eio.Promise.is_resolved t.closed then close c
    else
      let remade () =
        close c;
        match t.remake () with Ok fresh -> fresh | Error _ -> c
      in
      let c =
        if c.closed then remade ()
        else
          match rolled_back (off (fun () -> exec c.db "rollback")) with
          | Ok () -> c
          | Error e ->
              Log.err (fun m ->
                  m "a pooled connection was closed, its rollback failed: %s"
                    (S.error_to_string (Backend.error e)));
              remade ()
      in
      Eio.Stream.add t.free c

  let use ?wait t f =
    if Eio.Promise.is_resolved t.closed then Error (`Pool `Closed)
    else
      match take t ~wait:(Option.value wait ~default:t.wait) with
      | Error `Busy -> Error (`Pool `Busy)
      | Error `Closed -> Error (`Pool `Closed)
      | Ok c -> (
          match f c with
          | v ->
              Eio.Cancel.protect (fun () -> give_back t c);
              Ok v
          | exception ex ->
              let trace = Printexc.get_raw_backtrace () in
              Eio.Cancel.protect (fun () -> give_back t c);
              Printexc.raise_with_backtrace ex trace)

  type stats = { size : int; idle : int; waiting : int }

  let stats (t : t) =
    {
      size = t.size;
      idle = Eio.Stream.length t.free;
      waiting = Atomic.get t.waiting;
    }

  let close t =
    ignore (Eio.Promise.try_resolve t.close_now () : bool);
    let rec idle () =
      match Eio.Stream.take_nonblocking t.free with
      | Some c ->
          close c;
          idle ()
      | None -> ()
    in
    idle ()
end

(* ------------------------------------------------------------------ *)
(* Transactions *)

module Transaction = struct
  type behaviour = Deferred | Immediate | Exclusive

  type failure =
    [ `Not_committed of string
    | `Conflict of string option
    | `Not_serializable of string ]

  let begin_statement = function
    | Deferred -> "begin deferred"
    | Immediate -> "begin immediate"
    | Exclusive -> "begin exclusive"

  let not_committed what detail =
    Log.err (fun m -> m "a transaction was not committed: %s: %s" what detail);
    Error (`Not_committed (what ^ ": " ^ detail))

  (* A COMMIT and a ROLLBACK are made outside any cancellation: a cancelled
     fiber may not start a statement, and a transaction it left open would
     keep its lock on a connection its caller goes on using. *)
  let rollback db =
    match
      rolled_back (Eio.Cancel.protect (fun () -> Backend.script db "rollback"))
    with
    | Ok () -> ()
    | Error e ->
        Log.err (fun m ->
            m "a transaction could not be rolled back: %s"
              (S.error_to_string (Backend.error e)))

  (* A COMMIT SQLite refuses -- a lock it could not take, a deferred foreign
     key -- leaves the transaction open, so it is rolled back here; one
     SQLite had already rolled back answers that none is open. *)
  let commit db =
    match Eio.Cancel.protect (fun () -> exec_raw db "commit") with
    | Ok () -> Ok ()
    | Error e ->
        rollback db;
        Error e

  let ended = function
    | `Conflict name -> Error (`Conflict name)
    | `Not_serializable detail -> Error (`Not_serializable detail)
    | #S.error as e -> not_committed "commit" (S.error_to_string e)

  let once ~keep ~behaviour db work =
    match exec_raw db (begin_statement behaviour) with
    | Error (`Not_serializable detail) -> Error (`Not_serializable detail)
    | Error e -> not_committed "begin" (S.error_to_string e)
    | Ok () -> (
        match work db with
        | exception ex ->
            let trace = Printexc.get_raw_backtrace () in
            rollback db;
            Printexc.raise_with_backtrace ex trace
        | Ok v -> ( match commit db with Ok () -> Ok v | Error e -> ended e)
        | Error e when not (keep e) ->
            rollback db;
            Error e
        | Error e -> (
            match commit db with Ok () -> Error e | Error f -> ended f))

  (* The whole transaction again, from [begin] and at once; the bound is
     what keeps contention that does not clear from spinning. *)
  let within ?(keep = fun _ -> false) ?(behaviour = Deferred) ?(retries = 0) db
      work =
    let rec attempt left =
      match once ~keep ~behaviour db work with
      | Error (`Not_serializable detail) when left > 0 ->
          Log.info (fun m ->
              m "a transaction was not serializable, run again: %s" detail);
          attempt (left - 1)
      | answer -> answer
    in
    attempt retries

  type savepoint = Savepoint

  let savepoint_name = "rowtype_savepoint"

  let savepoint db =
    Result.map
      (fun () -> Savepoint)
      (exec_raw db ("savepoint " ^ savepoint_name))

  (* RELEASE answers SQLITE_ERROR, and nothing else, where the savepoint is
     not there; one that is there fails only as a COMMIT does, where it ends
     the transaction. *)
  let release db Savepoint =
    match Backend.script db ("release " ^ savepoint_name) with
    | Ok () -> Ok `Released
    | Error (Sqlite { code = Rc.ERROR; _ }) -> Ok `Gone
    | Error ((Sqlite _ | Closed | Refused _) as e) -> Error (Backend.error e)
end

(* ------------------------------------------------------------------ *)
(* Statements against the database *)

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
  | S.Enum _ -> "enum"

(* What SQLite turns a value into in a column declared as this, by its
   rules of affinity: the first of these the declared type holds. A column
   declared with no type takes any value as it came. *)
type affinity = Integer | Text | Blob | Real | Numeric

let affinity declared =
  let has part =
    let n = String.length part and h = String.length declared in
    let rec go i =
      i + n <= h && (String.equal (String.sub declared i n) part || go (i + 1))
    in
    go 0
  in
  if has "INT" then Integer
  else if has "CHAR" || has "CLOB" || has "TEXT" then Text
  else if has "BLOB" || String.equal declared "" then Blob
  else if has "REAL" || has "FLOA" || has "DOUB" then Real
  else Numeric

(* Which columns each scalar reads. Text a NUMERIC column keeps is any but
   text that looks like a number, which it makes one: no instant, date,
   interval or uuid does, and a text, a JSON document or a label may. *)
let reads : type a. a S.scalar -> affinity -> bool =
 fun scalar affinity ->
  match (scalar, affinity) with
  | ( ( S.Int | S.Int64 | S.Bool | S.Float | S.Text | S.Json | S.Enum _
      | S.Instant | S.Date | S.Timestamp | S.Interval | S.Uuid | S.Bytes ),
      Blob ) ->
      true
  | (S.Int | S.Int64 | S.Bool), (Integer | Numeric) -> true
  | (S.Int | S.Int64 | S.Bool), (Text | Real) -> false
  | S.Float, (Integer | Real | Numeric) -> true
  | S.Float, Text -> false
  | (S.Text | S.Json | S.Enum _), Text -> true
  | (S.Text | S.Json | S.Enum _), (Integer | Real | Numeric) -> false
  | (S.Instant | S.Date | S.Timestamp | S.Interval | S.Uuid), (Text | Numeric)
    ->
      true
  | (S.Instant | S.Date | S.Timestamp | S.Interval | S.Uuid), (Integer | Real)
    ->
      false
  | S.Bytes, (Integer | Text | Real | Numeric) -> false

(* What SQLite says of a statement it prepared and did not run: each
   placeholder's name, in order, and each column's declared type, where the
   column is a table's. *)
type described =
  | Refused_by of string
  | Described of string option list * string option list

let describe t sql =
  if t.closed then Error Closed
  else
    off (fun () ->
        match
          guarded t.db (fun () ->
              let* stmt = prepare t.db sql in
              let placeholders =
                List.init (Sqlite3.bind_parameter_count stmt) (fun i ->
                    Sqlite3.bind_parameter_name stmt (i + 1))
              and columns =
                List.init (Sqlite3.column_count stmt) (fun i ->
                    Sqlite3.column_decltype stmt i)
              in
              finalize stmt;
              Ok (Described (placeholders, columns)))
        with
        | Ok d -> Ok d
        (* A lock is the check's failure; anything else SQLite refused is
           the statement's. *)
        | Error (Sqlite { code = Rc.BUSY | Rc.LOCKED; _ } as f) -> Error f
        | Error (Sqlite { message; _ } | Refused message) ->
            Ok (Refused_by message)
        | Error Closed -> Error Closed)

let problems t (declared : S.declared) =
  let* d = describe t declared.sql in
  match d with
  | Refused_by message -> Ok [ "the database refuses it: " ^ message ]
  | Described (placeholders, columns) ->
      let parameters =
        match numbers placeholders (List.length declared.parameters) with
        | Ok (_ : int list) -> []
        | Error m -> [ m ]
      in
      let columns =
        match declared.row with
        | None -> []
        | Some row ->
            let declared_n = List.length row
            and actual_n = List.length columns in
            if declared_n <> actual_n then
              [
                Printf.sprintf "%d columns declared, and the database has %d"
                  declared_n actual_n;
              ]
            else
              List.concat
                (List.mapi
                   (fun i (column, decl) ->
                     match (column, decl) with
                     | _, None -> []
                     | S.Column s, Some decl ->
                         let decl = String.uppercase_ascii decl in
                         if reads s (affinity decl) then []
                         else
                           [
                             Printf.sprintf
                               "column %d is %s, which %s does not read" (i + 1)
                               decl (scalar_name s);
                           ]
                     | S.Array_of _, Some decl -> (
                         let decl = String.uppercase_ascii decl in
                         match affinity decl with
                         | Text | Blob -> []
                         | Integer | Real | Numeric ->
                             [
                               Printf.sprintf
                                 "column %d is %s, which an array, kept as \
                                  JSON text, is not"
                                 (i + 1) decl;
                             ]))
                   (List.combine row columns))
      in
      Ok (parameters @ columns)

let verify t statements =
  let rec each found = function
    | [] ->
        if List.is_empty found then Ok ()
        else Error (`Disagreements (List.rev found))
    | statement :: rest -> (
        let declared = S.declared statement in
        match problems t declared with
        | Error f -> Error (Backend.error f)
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

type error =
  [ S.error | `Disagreements of string list | Pool.error | Transaction.failure ]

let error_to_string : [< error ] -> string = function
  | `Disagreements problems ->
      "the database disagrees: " ^ String.concat "; " problems
  | `Pool `Busy -> "no connection came free within the wait"
  | `Pool `Closed -> "the pool is closed"
  | `Not_committed m -> "the transaction was not committed: " ^ m
  | #S.error as e -> S.error_to_string e

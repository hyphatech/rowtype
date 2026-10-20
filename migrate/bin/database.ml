(* A database the command was pointed at, of whichever dialect, as the
   operations its commands run: each dialect's library behind them, built
   where that dialect is installed, and the library's failures in its words. *)

module M = Rowtype_migrate

type t = {
  apply :
    lock:int option ->
    table:string ->
    libraries:M.source list ->
    M.migration list ->
    (unit, string) result;
  status :
    table:string ->
    libraries:M.source list ->
    M.migration list ->
    (M.status list, string) result;
  dump :
    lock:int option ->
    table:string ->
    libraries:M.source list ->
    pg_dump:string ->
    restrict_key:string ->
    M.migration list ->
    (string, string) result;
  squash :
    lock:int option ->
    table:string ->
    libraries:M.source list ->
    pg_dump:string ->
    restrict_key:string ->
    through:M.version ->
    M.migration list ->
    (M.migration, string) result;
  create : unit -> (unit, string) result;
  drop : unit -> (unit, string) result;
}

let told r = Result.map_error M.error_to_string r

(* [sqlite:] and a path, the two slashes of a URL taken off, so
   [sqlite:///srv/app.db] is the path [/srv/app.db]; [None] for a URL of
   any other dialect. *)
let sqlite_file url =
  let prefix = "sqlite:" in
  if String.starts_with ~prefix url then
    let path =
      String.sub url (String.length prefix)
        (String.length url - String.length prefix)
    in
    let path =
      if String.starts_with ~prefix:"//" path then
        String.sub path 2 (String.length path - 2)
      else path
    in
    Some
      (if String.equal path "" then Error "sqlite: names no file: sqlite:PATH"
       else Ok path)
  else None

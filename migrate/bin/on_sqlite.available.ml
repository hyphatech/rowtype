(* SQLite, where rowtype-sqlite is installed. *)

module Q = Rowtype_migrate_sqlite

let told = Database.told

(* A flag only Postgres reads, refused on a file rather than ignored, so
   nobody believes a run was serialised, or a program run, that was not. *)
let postgres_only flag why = function
  | Some _ -> Error (Printf.sprintf "%s is %s: leave it out" flag why)
  | None -> Ok ()

let no_pg_dump ~pg_dump ~restrict_key =
  Result.bind
    (postgres_only "--pg-dump" "Postgres's, and a SQLite dump runs no program"
       pg_dump) (fun () ->
      postgres_only "--restrict-key"
        "pg_dump's, and a SQLite dump runs no program" restrict_key)

let of_file file : Database.t =
  {
    apply =
      (fun ~lock ~table ~libraries migrations ->
        Result.bind
          (postgres_only "--lock"
             "Postgres's advisory lock, and a SQLite file has none" lock)
        @@ fun () ->
        told
          ( Eio_main.run @@ fun env ->
            Q.run env file (fun conn ->
                Q.apply ~table ~libraries conn migrations) ));
    status =
      (fun ~table ~libraries migrations ->
        told
          ( Eio_main.run @@ fun env ->
            Q.run env file (fun conn ->
                Q.status ~table ~libraries conn migrations) ));
    dump =
      (fun ~table ~libraries ~pg_dump ~restrict_key migrations ->
        Result.bind (no_pg_dump ~pg_dump ~restrict_key) @@ fun () ->
        told
          ( Eio_main.run @@ fun env ->
            Eio.Switch.run @@ fun sw ->
            Q.dump ~sw
              ~mono_clock:(Eio.Stdenv.mono_clock env)
              ~table ~libraries migrations ));
    squash =
      (fun ~table ~libraries ~pg_dump ~restrict_key ~through migrations ->
        Result.bind (no_pg_dump ~pg_dump ~restrict_key) @@ fun () ->
        told
          ( Eio_main.run @@ fun env ->
            Eio.Switch.run @@ fun sw ->
            Q.squash ~sw
              ~mono_clock:(Eio.Stdenv.mono_clock env)
              ~table ~libraries ~through migrations ));
    create =
      (fun () ->
        told
          ( Eio_main.run @@ fun env ->
            Eio.Switch.run @@ fun sw ->
            Q.create ~sw ~mono_clock:(Eio.Stdenv.mono_clock env) file ));
    drop = (fun () -> told (Q.drop file));
  }

let database url = Option.map (Result.map of_file) (Database.sqlite_file url)

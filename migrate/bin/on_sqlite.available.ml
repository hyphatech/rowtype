(* SQLite, where rowtype-sqlite is installed. *)

module Q = Rowtype_migrate_sqlite

let told = Database.told

let of_file file : Database.t =
  {
    apply =
      (fun ~lock:_ ~table ~libraries migrations ->
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
      (fun ~lock:_ ~table ~libraries ~pg_dump:_ ~restrict_key:_ migrations ->
        told
          ( Eio_main.run @@ fun env ->
            Eio.Switch.run @@ fun sw ->
            Q.dump ~sw
              ~mono_clock:(Eio.Stdenv.mono_clock env)
              ~table ~libraries migrations ));
    squash =
      (fun ~lock:_
        ~table
        ~libraries
        ~pg_dump:_
        ~restrict_key:_
        ~through
        migrations
      ->
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

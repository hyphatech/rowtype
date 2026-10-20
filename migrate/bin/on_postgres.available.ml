(* Postgres, where rowtype-postgres is installed: every URL that is not
   another dialect's, read as a connection string. *)

module P = Rowtype_migrate_postgres

let told = Database.told

(* What the commands that make, drop or dump a database are handed: the
   capabilities they use, on a switch the command's run owns. *)
let in_eio f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  f ~sw ~net:(Eio.Stdenv.net env)
    ~mono_clock:(Eio.Stdenv.mono_clock env)
    ~process_mgr:(Eio.Stdenv.process_mgr env)

let of_target target : Database.t =
  {
    apply =
      (fun ~lock ~table ~libraries migrations ->
        told
          ( Eio_main.run @@ fun env ->
            P.run env target (fun conn ->
                P.apply ?lock ~table ~libraries conn migrations) ));
    status =
      (fun ~table ~libraries migrations ->
        told
          ( Eio_main.run @@ fun env ->
            P.run env target (fun conn ->
                P.status ~table ~libraries conn migrations) ));
    dump =
      (fun ~lock ~table ~libraries ~pg_dump ~restrict_key migrations ->
        told
          (in_eio (fun ~sw ~net ~mono_clock ~process_mgr ->
               P.dump ~sw ~net ~mono_clock ~process_mgr ?lock ~table ~libraries
                 ~pg_dump ~restrict_key ~migrations target)));
    squash =
      (fun ~lock ~table ~libraries ~pg_dump ~restrict_key ~through migrations ->
        told
          (in_eio (fun ~sw ~net ~mono_clock ~process_mgr ->
               P.squash ~sw ~net ~mono_clock ~process_mgr ?lock ~table
                 ~libraries ~pg_dump ~restrict_key ~through ~migrations target)));
    create =
      (fun () ->
        told
          (in_eio (fun ~sw ~net ~mono_clock ~process_mgr:_ ->
               P.create ~sw ~net ~mono_clock target)));
    drop =
      (fun () ->
        told
          (in_eio (fun ~sw ~net ~mono_clock ~process_mgr:_ ->
               P.drop ~sw ~net ~mono_clock target)));
  }

let database url =
  match Database.sqlite_file url with
  | Some _ -> None
  | None ->
      Some (Result.map of_target (Rowtype_postgres.Conninfo.of_string url))

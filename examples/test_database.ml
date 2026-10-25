module Pg = Rowtype_postgres

let with_test_database env target test =
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env and mono_clock = Eio.Stdenv.mono_clock env in
  let target = Pg.Conninfo.with_database target "shop_test" in
  let ( let* ) = Result.bind in
  let* () = Rowtype_migrate_postgres.create ~sw ~net ~mono_clock target in
  let result =
    let* () = Migrate_postgres.migrate env target in
    test target
  in
  let* () = Rowtype_migrate_postgres.drop ~sw ~net ~mono_clock target in
  result

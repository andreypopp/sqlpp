let () =
  let env = Sqlpp_sqlite.env in
  Sqlpp.Env.add env [%blob "test/schema.sql"];
  Ppxlib.Driver.standalone ()

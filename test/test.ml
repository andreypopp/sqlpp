let () =
  let env = Sqlpp_sqlite.env in
  Sqlpp.Env.add env [%blob "test/schema.sql"];
  Sqlpp_manage.Command_line_interface.run ()

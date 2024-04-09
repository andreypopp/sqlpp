let () =
  let env = Sqlpp_sqlite.env in
  Sqlpp.Env.add env [%blob "test/sqlpp/schema.sql"];
  Sqlpp_sqlite_manage.Command_line_interface.run ()

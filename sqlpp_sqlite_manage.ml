open Sqlpp_sqlite

module Sqlpp_migrate = struct
  open Sqlpp_db.IO

  type db = Sqlpp_db.db
  type 'a io = 'a Sqlpp_db.IO.t

  let run_io f = f ()
  let ddl_to_string = ddl_to_string

  [%%sqlpp.env
  {|
     create table _migrate (name string not null);
     create table sqlite_master (name string not null, type string not null);
  |}]

  let init db =
    let sql =
      "create table if not exists _migrate(name string not null primary key)"
    in
    Sqlpp_db.exec db { sql }

  let parse_bool = function Some false | None -> false | Some true -> true

  let is_init db =
    parse_bool
    @@ [%sqlpp.fetch_option
         "SELECT true FROM sqlite_master WHERE type='table' AND \
          name='_migrate'"] db

  let exists db name =
    parse_bool
    @@ [%sqlpp.fetch_option
         "SELECT true FROM _migrate WHERE name = ?name:string not null"] ~name
         db

  let record db name =
    [%sqlpp.exec "insert into _migrate(name) values(?name:string not null)"]
      ~name db
end

include Sqlpp_manage.Make (Sqlpp_db) (Sqlpp_types) (Sqlpp_migrate)

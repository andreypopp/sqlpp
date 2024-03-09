open Sqlpp
open Sqlpp_sqlite

module Migrate_db : sig
  val init : Sqlite3.db -> unit
  val exists : Sqlite3.db -> string -> bool
  val record : Sqlite3.db -> string -> unit
end = struct
  [%%sqlpp.env
  {|
  create table _migrate (name string not null);
  create table sqlite_master (name string not null, type string not null);
  |}]

  let init db =
    Sqlpp_db.exec db
      {
        sql =
          {|create table if not exists _migrate(name string not null primary key)|};
      }

  let exists db name =
    [%sqlpp.query
      "select true from _migrate where name = ?name:string not null"] ~name ()
    |> Sqlpp_db.fold db ~init:false ~f:(fun ~_0:_ _ -> true)

  let record db name =
    [%sqlpp.query "insert into _migrate(name) values(?name:string not null)"]
      ~name ()
    |> Sqlpp_db.exec db
end

module Migrate = struct
  type action = Define of Ddl.t | Exec of string

  let execute query = Exec query

  let create_table ?(primary_key = []) ~table columns =
    Define (CREATE_TABLE { name = table; columns; primary_key })

  let drop_table ~table = Define (DROP_TABLE { table })

  let alter_table_rename ~table ~new_table =
    Define (ALTER_TABLE_RENAME { table; new_table })

  let alter_table_rename_column ~table ~column ~new_column =
    Define (ALTER_TABLE_RENAME_COLUMN { table; column; new_column })

  let alter_table_drop_column ~table ~column =
    Define (ALTER_TABLE_DROP_COLUMN { table; column })

  let alter_table_add_column ?default ~table column =
    Define (ALTER_TABLE_ADD_COLUMN { table; column; default })

  let column ?extra ?default ?primary_key ?autoincrement name ty =
    Ddl.column ?extra ?default ?primary_key ?autoincrement name ty

  type t = { name : string; actions : action list }

  let migrations : t list ref = ref []
  let all () = List.rev !migrations |> List.to_seq

  let action_to_env env = function
    | Define ddl -> Env.add_ddl env ddl
    | Exec q -> ()

  let to_env env m = List.iter m.actions ~f:(action_to_env env)

  let find name =
    let env = Env.create () in
    Seq.find_map
      (fun m ->
        if String.equal m.name name then Some (m, env)
        else (
          to_env env m;
          None))
      (all ())

  let migrate name actions = migrations := { name; actions } :: !migrations

  let setup_env_of env m =
    List.iter m.actions ~f:(function
      | Define t -> Env.add_ddl env t
      | Exec _ -> ())

  let setup_env env = Seq.iter (setup_env_of env) (all ())

  let action_to_sql env = function
    | Define ddl ->
        Env.add_ddl env ddl;
        ddl_to_string env ddl
    | Exec q -> sprintf "%s;" (Sqlpp_db.Dynamic.to_sql env q)

  let to_sql env { actions; name } =
    [ sprintf "-- MIGRATE: %s" name; "BEGIN;" ]
    @ List.map actions ~f:(action_to_sql env)
    @ [ "COMMIT;" ]
    |> String.concat ~sep:"\n"

  let exec ~verbose db sql =
    if verbose then print_endline sql;
    try
      let rc = Sqlite3.exec db sql in
      Sqlite3.Rc.check rc
    with Sqlite3.SqliteError err -> failwith (Sqlite3.errmsg db)

  let apply ~verbose db migrations =
    Migrate_db.init db;
    let env = Env.create () in
    Seq.iter
      (fun m ->
        let applied = Migrate_db.exists db m.name in
        let sql = to_sql env m in
        if not applied then (
          exec ~verbose db sql;
          Migrate_db.record db m.name))
      migrations

  let is_ready db migrations =
    match
      [%sqlpp.fetch_option
        "SELECT true FROM sqlite_master WHERE type='table' AND name='_migrate'"]
        db
    with
    | Some false | None -> false
    | Some true ->
        Seq.for_all (fun m -> Migrate_db.exists db m.name) migrations

  let apply ?(verbose = false) db = apply ~verbose db (all ())
  let is_ready db = is_ready db (all ())
end

let setup_env = Migrate.setup_env

module Command_line_interface = struct
  open Cmdliner

  let handle_error f =
    try f ()
    with Sqlpp.Report.Error report ->
      Format.eprintf "ERROR: %a@." Sqlpp.Report.pp report;
      Stdlib.exit 1

  let verbose_t = Arg.(value & flag & info [ "verbose" ] ~doc:"verbose output")

  let db_t =
    let env = Cmd.Env.info "DATABASE" in
    let arg =
      Arg.(
        value
        & opt string ":memory:"
        & info [ "D"; "database" ] ~env ~doc:"database" ~docv:"DATABASE")
    in
    let f db =
      let db = Sqlite3.db_open db in
      Migrate_db.init db;
      db
    in
    Term.(const f $ arg)

  let query_t =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"QUERY")

  let migration_t =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"MIGRATION")

  let require_t =
    let arg =
      Arg.(value & opt_all string [] & info [ "require" ] ~docv:"REQ")
    in
    let f =
      List.map ~f:(fun req ->
          if Sys.file_exists req then `file req else `src req)
    in
    Term.(const f $ arg)

  let params_t : Sqlpp.params Term.t =
    let arg =
      Arg.(
        value & opt_all string [] & info [ "param"; "p" ] ~docv:"NAME=VALUE")
    in
    let f =
      List.fold_left ~init:Syntax.NM.empty ~f:(fun params p ->
          match String.split_on_char ~by:'=' p with
          | n :: vs ->
              let v = String.concat ~sep:"=" vs in
              Syntax.(NM.add (dummy_loc, n)) v params
          | _ -> failwith "invalid parameter, should be NAME=VALUE")
    in
    Term.(const f $ arg)

  let env_t =
    let f requires =
      handle_error @@ fun () ->
      let env = Env.create () in
      Migrate.setup_env env;
      List.iter requires ~f:(function
        | `src src -> Env.add env src
        | `file file -> Env.add_file env file);
      env
    in
    Term.(const f $ require_t)

  let s_sections = ref []

  let define_section name =
    s_sections := `S name :: !s_sections;
    name

  let docs = define_section "DATABASE MIGRATIONS"

  let migrate_cmd =
    let f verbose db = Migrate.apply ~verbose db in
    let info =
      Cmd.info "migrate" ~docs
        ~doc:"apply all pending migrations to the database"
    in
    Cmd.v info Term.(const f $ verbose_t $ db_t)

  let migrate_ls_cmd =
    let f db =
      print_endline (sprintf "status\tname");
      Migrate.all ()
      |> Seq.iter (fun { Migrate.name; _ } ->
             let status = Migrate_db.exists db name in
             print_endline (sprintf "%b\t%s" status name))
    in
    let info =
      Cmd.info "migrate-ls" ~docs ~doc:"list all migrations and their status"
    in
    Cmd.v info Term.(const f $ db_t)

  let migrate_show_cmd =
    let f _db name =
      match Migrate.find name with
      | None ->
          prerr_endline (sprintf "migration not found: %s" name);
          exit 1
      | Some (m, env) -> print_endline (Migrate.to_sql env m)
    in
    let info =
      Cmd.info "migrate-show" ~docs ~doc:"show the SQL of a migration"
    in
    Cmd.v info Term.(const f $ db_t $ migration_t)

  let docs = define_section "DATABASE INTROSPECTION"

  let query_cmd =
    let f db env params q =
      handle_error @@ fun () ->
      let sql = Sqlpp_db.Dynamic.to_sql ~params env q in
      print_endline sql;
      let res = Sqlpp_db.Dynamic.exec ~params env db q in
      List.iter res ~f:(fun row ->
          print_endline (Yojson.Basic.pretty_to_string row))
    in
    Cmd.(v (info "sqlpp-query" ~docs ~doc:"execute query"))
      Term.(const f $ db_t $ env_t $ params_t $ query_t)

  let docs = define_section "DATABASE ADVANCED COMMANDS"

  let analyze_cmd =
    let f _db env src =
      handle_error @@ fun () ->
      let q = parse_query src in
      let q = Analyze.analyze_query ~src env q in
      let buf = Buffer.create 100 in
      PPrint.ToBuffer.pretty 0.8 60 buf
        (Sqlpp.print_query ~scope:q.inner_scope q.query);
      print_endline (Buffer.contents buf)
    in
    Cmd.(
      v
        (info "sqlpp-analyze" ~docs
           ~doc:"parse, analyze and print elaborated query"))
      Term.(const f $ db_t $ env_t $ query_t)

  let fmt_cmd =
    let f _db q =
      handle_error @@ fun () ->
      let q = parse_query q in
      let buf = Buffer.create 100 in
      PPrint.ToBuffer.pretty 0.8 60 buf (Sqlpp.print_query q);
      print_endline (Buffer.contents buf)
    in
    Cmd.(v (info "sqlpp-fmt" ~docs ~doc:"parse and print query"))
      Term.(const f $ db_t $ query_t)

  let sql_cmd =
    let f _db env params q =
      handle_error @@ fun () ->
      let sql = Sqlpp_db.Dynamic.to_sql ~params env q in
      print_endline sql
    in
    Cmd.(
      v
        (info "sqlpp-sql" ~docs
           ~doc:"parse, analyze and translate query to SQL"))
      Term.(const f $ db_t $ env_t $ params_t $ query_t)

  let commands =
    [
      query_cmd;
      analyze_cmd;
      fmt_cmd;
      sql_cmd;
      migrate_cmd;
      migrate_ls_cmd;
      migrate_show_cmd;
    ]

  let s_sections = List.rev !s_sections

  let main_cmd ~name =
    let info = Cmd.info name ~version:"%%VERSION%%" in
    let default =
      Term.(ret (const (fun () -> `Help (`Pager, None)) $ const ()))
    in
    Cmd.group info ~default commands

  let run ?(name = "sqlpp") () = Stdlib.exit (Cmd.eval (main_cmd ~name))
end

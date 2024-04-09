open Sqlpp.Syntax

module M = Sqlpp_sqlite_manage.Migrate

let default_project_id = "default"

(* as the migration framework doesn't support changin the column type now (in
   part because sqlite doesn't support it as well), we implement it manually by
   creating a new column, copying the data, and then dropping the old column *)
let update_column_type ?update ?default table column new_type =
  let column_prev = column ^ "_prev" in
  let update =
    match update with None -> column_prev | Some update -> update column_prev
  in
  [
    M.alter_table_rename_column ~table ~column ~new_column:column_prev;
    M.alter_table_add_column ?default ~table (M.column column new_type);
    M.execute (Printf.sprintf "UPDATE %s SET %s = %s" table column update);
    M.alter_table_drop_column ~table ~column:column_prev;
  ]

let () =
  M.migrate "create todos"
    [
      M.create_table ~table:"todos"
        [
          M.column "id" (non_null int) ~primary_key:true ~autoincrement:true;
          M.column "text" (null string);
          M.column "created" (non_null float);
          M.column "done" (non_null bool);
        ];
    ];
  M.migrate "todos: done -> completed"
    [
      M.alter_table_rename_column ~table:"todos" ~column:"done"
        ~new_column:"completed";
    ];
  M.migrate "todos: text null -> text not null"
    (update_column_type "todos" "text" (non_null string)
       ~update:(Printf.sprintf "coalesce(%s, '')"));
  M.migrate "todos: created float -> created text"
    (update_column_type "todos" "created" (non_null string)
       ~update:(fun _prev -> "toString(now())"));
  M.migrate "create projects"
    [
      M.create_table ~table:"projects"
        [
          M.column "name" (non_null string) ~primary_key:true;
          M.column "created" (non_null string);
        ];
      M.execute
        (Printf.sprintf
           "INSERT INTO projects (name, created) VALUES ('%s', \
            toString(now()))"
           default_project_id);
      M.alter_table_add_column ~table:"todos"
        (M.column "project" (non_null string)
           ~default:(Printf.sprintf "'%s'" default_project_id));
    ];
  M.migrate "todos/projects: created text -> created datetime"
    (update_column_type "todos" "created" (non_null datetime)
       ~default:"datetime('1970-01-01T00:00:00')" ~update:(fun prev ->
         Printf.sprintf "coalesce(datetime(%s), now())" prev)
    @ update_column_type "projects" "created" (non_null datetime)
        ~default:"datetime('1970-01-01T00:00:00')" ~update:(fun prev ->
          Printf.sprintf "coalesce(datetime(%s), now())" prev))

let () = Sqlpp_sqlite_manage.setup_env Sqlpp_sqlite.env

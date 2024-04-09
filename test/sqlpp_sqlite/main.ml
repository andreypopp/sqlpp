open Sqlpp_sqlite

type db = Sqlpp_db.db
(** just an alias for the database type *)

(** The [Todos] module provides an API for a todo list stored in a
    SQLite database. *)
module Todos : sig
  type t = {
    id : int;
    text : string;
    completed : bool;
    created : float;
    project : string;
  }

  val list : ?project:string -> db -> t list
  (** [list ?project db] returns all todos in the database [db] *)

  val find : id:int -> db -> t option
  (** [find ~id db] returns the todo with the given [id] from the database [db] *)

  val insert : ?completed:bool -> ?project:string -> text:string -> db -> int
  (** [insert ~completed ~text db] inserts a new todo with the given [text] and [completed] status into the database [db] *)

  val completed : id:int -> db -> unit
  (** [completed ~id db] marks the todo with the given [id] as completed in the database [db] *)

  val delete : id:int -> db -> unit
  (** [delete ~id db] deletes the todo with the given [id] from the database [db] *)

  type project_stats = {
    name : string;
    total_count : int;
    pending_count : int;
  }

  val project_stats : db -> project_stats list
  (** [project_stats db] returns the number of todos for each project in the database [db] *)

  val to_string : t -> string
  (** [to_string t] returns a string representation of the todo [t] *)
end = struct
  type t = {
    id : int;
    text : string;
    completed : bool;
    created : float;
    project : string;
  }
  (** a record type for todos *)

  [%%sqlpp.env
  {|
    CREATE FIELDSET todos_fields(from todos) AS
    SELECT
      id as id,
      text as text,
      completed as completed,
      created as created,
      project as project
  |}]
  (* now we define fieldset which corresponds to the record type *)

  let format_datetime_gmt v =
    let tm = Unix.gmtime v in
    Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d" (tm.tm_year + 1900)
      (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

  let to_string { id; text; completed; created; project } =
    Printf.sprintf "%d: [%s] %s (%s) #%s" id
      (if completed then "x" else " ")
      text
      (format_datetime_gmt created)
      project

  let list ?project db =
    let project = match project with None -> `None | Some p -> `Some p in
    [%fetch_list
      {|SELECT ...todos_fields(todos) 
        FROM todos
        WHERE MATCH ?project WITH
        | None -> TRUE 
        | Some ?project -> project = ?project:string not null
        END
        ORDER BY id ASC|}
        ~record:t]
      db ~project

  let find =
    [%fetch_option
      "SELECT ...todos_fields(todos) FROM todos WHERE id=?id:int not null"
        ~record:t]

  let insert_project =
    [%exec
      "INSERT INTO projects SET name=?name, created=now() ON CONFLICT IGNORE"]

  let insert ?(completed = false) ?(project = Db.default_project_id) ~text db =
    insert_project ~name:project db;
    [%fetch_option
      "INSERT INTO todos SET text=?text, completed=?completed, \
       project=?project, created=now() RETURNING id"]
      ~text ~completed ~project db
    |> Option.get

  let completed =
    [%exec "UPDATE todos SET completed=TRUE WHERE id=?id:int not null"]

  let delete = [%exec "DELETE FROM todos WHERE id=?id:int not null"]

  type project_stats = {
    name : string;
    total_count : int;
    pending_count : int;
  }

  [%%sqlpp.env
  {|
    CREATE QUERY project_stats AS
    SELECT ...
    FROM projects
    LEFT JOIN (select project, ... FROM todos GROUP BY project) AS todos 
    ON projects.name = todos.project;
  |}]
  (* just for the sake of demonstration, we are going to define a reusable
     query which we can use later. *)

  let project_stats =
    [%fetch_list
      {|
      SELECT 
        withscope project_stats.projects as projects,
        withscope project_stats.todos as todos,
        projects.name as name,
        coalesce(todos.count(1), 0) as total_count,
        coalesce(todos.count(nullif(completed, true)), 0) as pending_count
      FROM project_stats
      |}
        ~record:project_stats]
end

open Cmdliner

let db_t =
  let env = Cmd.Env.info "DATABASE" in
  let arg =
    Arg.(
      value
      & opt string "sqlite::memory:"
      & info [ "D"; "database" ] ~env ~doc:"database" ~docv:"DATABASE")
  in
  let f db_path =
    let db = Sqlpp_db.connect db_path in
    if not (Sqlpp_sqlite_manage.Migrate.is_ready db) then (
      let msg =
        Printf.sprintf
          "ERROR: pending migrations found, run\n\n\
          \  %s migrate -D %S\n\n\
           to apply them" Sys.argv.(0) db_path
      in
      prerr_endline msg;
      exit 1);
    db
  in
  Term.(const f $ arg)

let text_t = Arg.(required & pos 0 (some string) None & info [])
let id_t = Arg.(required & pos 0 (some int) None & info [])

let project_t =
  Arg.(
    value
    & opt (some string) None
    & info [ "P"; "project" ] ~doc:"project to associate the todo with "
        ~docv:"PROJECT")

let completed_t =
  Arg.(value & flag & info [ "completed" ] ~doc:"show completed todos as well")

let cmd ~name ~doc t = Cmd.v (Cmd.info name ~doc) t

let todos_insert_cmd =
  let f db project text =
    let id = Todos.insert ?project ~text db in
    print_endline (Printf.sprintf "TODO %i inserted" id)
  in
  cmd ~name:"todos-insert" ~doc:"insert a new todo into a database"
    Term.(const f $ db_t $ project_t $ text_t)

let todos_completed_cmd =
  let f db id = Todos.completed ~id db in
  cmd ~name:"todos-completed" ~doc:"mark todo as completed"
    Term.(const f $ db_t $ id_t)

let todos_delete_cmd =
  let f db id = Todos.delete ~id db in
  cmd ~name:"todos-delete" ~doc:"delete a todo" Term.(const f $ db_t $ id_t)

let todos_show_cmd =
  let f db id =
    match Todos.find ~id db with
    | Some t -> print_endline (Todos.to_string t)
    | None -> Printf.printf "todo with id %d not found\n" id
  in
  cmd ~name:"todos-show" ~doc:"show a todo" Term.(const f $ db_t $ id_t)

let todos_ls_cmd =
  let f db project show_completed =
    Todos.list ?project db
    |> List.iter (fun t ->
           if show_completed || not t.Todos.completed then
             print_endline (Todos.to_string t))
  in
  cmd ~name:"todos-ls" ~doc:"list todos"
    Term.(const f $ db_t $ project_t $ completed_t)

let todos_project_stats_cmd =
  let f db =
    Todos.project_stats db
    |> List.iter (fun t ->
           print_endline
             (Printf.sprintf "%s: %d/%d pending/total" t.Todos.name
                t.pending_count t.total_count))
  in
  cmd ~name:"todos-project-stats" ~doc:"show project stats"
    Term.(const f $ db_t)

let commands =
  [
    todos_ls_cmd;
    todos_insert_cmd;
    todos_delete_cmd;
    todos_completed_cmd;
    todos_show_cmd;
    todos_project_stats_cmd;
  ]
  @ Sqlpp_sqlite_manage.Command_line_interface.commands

let main_cmd =
  let info =
    Cmd.info
      ~man:
        ([ `S Manpage.s_commands ]
        @ Sqlpp_sqlite_manage.Command_line_interface.s_sections)
      "example" ~version:"%%VERSION%%"
  in
  Cmd.group info commands

let () = Stdlib.exit (Cmd.eval main_cmd)

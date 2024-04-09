(** Basic database migration library.

    Migrations are defined as a sequence of actions that are applied to a
    database within a single transaction.

    An action can be one of the following:
    - DDL statement (create table, drop table, etc.)
    - An arbitrary query (insert, update, delete, etc.)
 *)

open Sqlpp

module type MIGRATE = sig
  type db
  type 'a io

  val run_io : (unit -> 'a io) -> 'a
  val ddl_to_string : Env.t -> Ddl.t -> string
  val init : db -> unit io
  val is_init : db -> bool io
  val exists : db -> string -> bool io
  val record : db -> string -> unit io
end

module Make
    (Sqlpp_db : BACKEND)
    (Sqlpp_types : TYPES with type row = Sqlpp_db.row)
    (Migrate : MIGRATE
                 with type 'a io = 'a Sqlpp_db.IO.t
                  and type db = Sqlpp_db.db) : sig
  module Migrate : sig
    type action

    val column :
      ?extra:string ->
      ?default:string ->
      ?primary_key:bool ->
      ?autoincrement:bool ->
      string ->
      Syntax.ty ->
      Ddl.column
    (** [column ?extra ?default ?primary_key ?autoincrement name ty] creates a
      new column with the given name and type. *)

    val create_table :
      ?primary_key:string list -> table:string -> Ddl.column list -> action
    (** [create_table ?primary_key ~table columns] creates a table with the given
      name [table] and [columns]. *)

    val drop_table : table:string -> action
    (** [drop_table ~table] drops the table with the given name [table]. *)

    val alter_table_rename : table:string -> new_table:string -> action
    (** [alter_table_rename ~table ~new_table] renames the table [table] to
      [new_table]. *)

    val alter_table_add_column :
      ?default:string -> table:string -> Ddl.column -> action
    (** [alter_table_add_column ?default ~table column] adds the column [column] to the
      table [table]. *)

    val alter_table_drop_column : table:string -> column:string -> action
    (** [alter_table_drop_column ~table ~column] drops the column [column] from
      the table [table]. *)

    val alter_table_rename_column :
      table:string -> column:string -> new_column:string -> action
    (** [alter_table_rename_column ~table ~column ~new_column] renames the
      column [column] to [new_column] in the table [table]. *)

    val execute : string -> action
    (** [execute query] executes the given [query]. 

      This is useful either to seed the database with initial data or to
      perform data migrations.
    *)

    val migrate : string -> action list -> unit
    (** [mirate name actions] registers a new migration named [name] with the
      actions [actions].

      The actions are applied in the order they are given, within a single
      transaction.
    *)

    val apply : ?verbose:bool -> Sqlpp_db.db -> unit Sqlpp_db.IO.t
    (** [apply db] applies all the migrations to the database [db] *)

    val is_ready : Sqlpp_db.db -> bool Sqlpp_db.IO.t
    (** [is_ready db] returns [true] if the database [db] is up to date
      i.e. if all the migrations have been applied. *)
  end

  val setup_env : Env.t -> unit
  (** [setup_env env] sets up the environment from the registered migrations. *)

  (** Command line interface. *)
  module Command_line_interface : sig
    val run : ?name:string -> unit -> unit
    (** An entry point.

      Alternatively, you can use the [commands] and [s_sections] values to
      embed the commands within your own command line interface.
   *)

    val db_t : Sqlpp_db.db Cmdliner.Term.t
    (** A term which evaluates to a database connection. *)

    val commands : unit Cmdliner.Cmd.t list
    (** A set of commands, in case you want to use them in your own command line
      interface.

      Example:

      {[
        let commands = 
          [ my_command ] @
          Sqlpp_manage.Command_line_interface.commands 
        in
        let info = ... in
        Cmd.group info commands
      ]}
   *)

    val s_sections : Cmdliner.Manpage.block list
    (** A list of man page sections. 

      If you embed the commands within your own command line interface, you
      might want to use this to control the order of man sections:

      {[
        let man =
          [ `S Manpage.s_commands ]
          @ Sqlpp_manage.Command_line_interface.s_sections
        in
        let info = Cmd.info ~man ... in
        Cmd.group info commands
      ]}
   *)
  end
end

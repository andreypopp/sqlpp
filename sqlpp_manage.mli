(** Basic database migration library.

    Migrations are defined as a sequence of actions that are applied to a
    database within a single transaction.

    An action can be one of the following:
    - DDL statement (create table, drop table, etc.)
    - An arbitrary query (insert, update, delete, etc.)
 *)

open Sqlpp

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

  val apply : ?verbose:bool -> Sqlite3.db -> unit
  (** [apply db] applies all the migrations to the database [db] *)

  val is_ready : Sqlite3.db -> bool
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

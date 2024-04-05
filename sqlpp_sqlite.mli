open Sqlpp

(** Database backend interface *)
module Sqlpp_db :
  BACKEND
    with type Db.db = Sqlite3.db
     and type Db.date = float
     and type Db.datetime = float

val env : Env.t
(** Default database environment. *)

val ddl_to_string : Env.t -> Ddl.t -> string
(** Convert a DDL statement to a string *)

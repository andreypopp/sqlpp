open Sqlpp
module M = Mariadb.Blocking

(** Database backend interface *)
module Sqlpp_db :
  BACKEND
    with type Db.db = M.t
     and type Db.date = M.Time.t
     and type Db.datetime = M.Time.t

val env : Env.t

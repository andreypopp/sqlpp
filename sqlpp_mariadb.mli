open Sqlpp

module Mariadb_lwt : Mariadb.Nonblocking.S with type 'a future := 'a Lwt.t
(** Mariadb on Lwt *)

module IO : IO with type 'a t = 'a Mariadb_lwt.result Lwt.t
(** IO monad *)

(** Database backend interface *)
module Sqlpp_db :
  BACKEND
    with type Db.db = Mariadb_lwt.t
     and type Db.date = Mariadb_lwt.Time.t
     and type Db.datetime = Mariadb_lwt.Time.t
     and module IO = IO

val env : Env.t
(** Default database environment. *)

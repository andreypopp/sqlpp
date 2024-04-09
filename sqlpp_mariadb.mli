open Sqlpp

module Mariadb_lwt : Mariadb.Nonblocking.S with type 'a future := 'a Lwt.t
(** Mariadb on Lwt *)

module IO : IO with type 'a t = 'a Mariadb_lwt.result Lwt.t
(** IO monad *)

(** Database backend interface *)
module Sqlpp_db :
  BACKEND
    with type db = Mariadb_lwt.t
     and type row = Mariadb_lwt.Row.Array.t
     and module IO = IO

(** Database types encoders/decoders. *)
module Sqlpp_types : sig
  include TYPES with type row = Sqlpp_db.row

  type 'a encode := 'a -> string
  type 'a decode := row -> int -> 'a

  (** DATE *)

  val encode_DATE : Mariadb_lwt.Time.t encode
  val decode_DATE : Mariadb_lwt.Time.t decode
  val encode_DATE_NULL : Mariadb_lwt.Time.t option encode
  val decode_DATE_NULL : Mariadb_lwt.Time.t option decode

  (** DATETIME *)

  val encode_DATETIME : Mariadb_lwt.Time.t encode
  val decode_DATETIME : Mariadb_lwt.Time.t decode
  val encode_DATETIME_NULL : Mariadb_lwt.Time.t option encode
  val decode_DATETIME_NULL : Mariadb_lwt.Time.t option decode
end

val env : Env.t
(** Default database environment. *)

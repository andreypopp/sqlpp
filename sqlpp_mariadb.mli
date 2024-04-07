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
  type row = Sqlpp_db.row
  type 'a encode := 'a -> string
  type 'a decode := row -> int -> 'a

  val encode_BOOL : bool encode
  val encode_BOOL_NULL : bool option encode
  val encode_INT : int encode
  val encode_FLOAT : float encode
  val encode_STRING : string encode
  val encode_DATE : Mariadb_lwt.Time.t encode
  val encode_DATETIME : Mariadb_lwt.Time.t encode
  val encode_INT_NULL : int option encode
  val encode_FLOAT_NULL : float option encode
  val encode_STRING_NULL : string option encode
  val encode_DATE_NULL : Mariadb_lwt.Time.t option encode
  val encode_DATETIME_NULL : Mariadb_lwt.Time.t option encode
  val decode_BOOL : bool decode
  val decode_INT : int decode
  val decode_FLOAT : float decode
  val decode_STRING : string decode
  val decode_DATE : Mariadb_lwt.Time.t decode
  val decode_DATETIME : Mariadb_lwt.Time.t decode
  val decode_BOOL_NULL : bool option decode
  val decode_INT_NULL : int option decode
  val decode_FLOAT_NULL : float option decode
  val decode_STRING_NULL : string option decode
  val decode_DATE_NULL : Mariadb_lwt.Time.t option decode
  val decode_DATETIME_NULL : Mariadb_lwt.Time.t option decode
end

val env : Env.t
(** Default database environment. *)

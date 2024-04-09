open Sqlpp

module IO : IO with type 'a t = 'a
(** Sqlite3 I/O is blocking, error are raised as failures. *)

(** Database backend interface. *)
module Sqlpp_db :
  BACKEND
    with type db = Sqlite3.db
     and type row = Sqlite3.Data.t array
     and module IO = IO

(** Database types encoders/decoders. *)
module Sqlpp_types : sig
  include TYPES with type row = Sqlpp_db.row

  type 'a encode := 'a -> string
  type 'a decode := row -> int -> 'a

  (** DATE *)

  val encode_DATE : float encode
  val decode_DATE : float decode
  val encode_DATE_NULL : float option encode
  val decode_DATE_NULL : float option decode

  (** DATETIME *)

  val encode_DATETIME : float encode
  val decode_DATETIME : float decode
  val encode_DATETIME_NULL : float option encode
  val decode_DATETIME_NULL : float option decode
end

val env : Env.t
(** Default database environment. *)

val ddl_to_string : Env.t -> Ddl.t -> string
(** Convert a DDL statement to a string *)

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
  type row = Sqlpp_db.row
  type 'a encode := 'a -> string
  type 'a decode := row -> int -> 'a

  val encode_BOOL : bool encode
  val encode_BOOL_NULL : bool option encode
  val encode_INT : int encode
  val encode_FLOAT : float encode
  val encode_STRING : string encode
  val encode_DATE : float encode
  val encode_DATETIME : float encode
  val encode_INT_NULL : int option encode
  val encode_FLOAT_NULL : float option encode
  val encode_STRING_NULL : string option encode
  val encode_DATE_NULL : float option encode
  val encode_DATETIME_NULL : float option encode
  val decode_BOOL : bool decode
  val decode_INT : int decode
  val decode_FLOAT : float decode
  val decode_STRING : string decode
  val decode_DATE : float decode
  val decode_DATETIME : float decode
  val decode_BOOL_NULL : bool option decode
  val decode_INT_NULL : int option decode
  val decode_FLOAT_NULL : float option decode
  val decode_STRING_NULL : string option decode
  val decode_DATE_NULL : float option decode
  val decode_DATETIME_NULL : float option decode
end

val env : Env.t
(** Default database environment. *)

val ddl_to_string : Env.t -> Ddl.t -> string
(** Convert a DDL statement to a string *)

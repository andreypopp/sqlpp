open Sqlpp

(** Postgresql on Lwt *)
module Postgresql_lwt : sig
  val connect :
    uri:string -> unit -> (Postgresql.connection, string) result Lwt.t
end

(** A single row in a result set. 

    Note that values of this type are mutable and not meant to be stored
    outside the scope of a value decoder.
  *)
module Row : sig
  type t

  val isnull : t -> int -> bool
  val get : t -> int -> string
  val ftype : t -> int -> Postgresql.ftype
end

module IO : IO with type 'a t = ('a, string) result Lwt.t
(** IO monad *)

(** Database backend interface *)
module Sqlpp_db :
  BACKEND
    with type db = Postgresql.connection
     and type row = Row.t
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
  val encode_INT_NULL : int option encode
  val encode_FLOAT_NULL : float option encode
  val encode_STRING_NULL : string option encode
  val decode_BOOL : bool decode
  val decode_INT : int decode
  val decode_FLOAT : float decode
  val decode_STRING : string decode
  val decode_BOOL_NULL : bool option decode
  val decode_INT_NULL : int option decode
  val decode_FLOAT_NULL : float option decode
  val decode_STRING_NULL : string option decode
end

val env : Env.t
(** Default database environment. *)

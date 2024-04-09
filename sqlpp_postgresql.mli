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
  include TYPES with type row = Sqlpp_db.row
end

val env : Env.t
(** Default database environment. *)

module type CONFIG = sig
  module Sqlpp_db : Sqlpp.BACKEND
end

module Make (C : CONFIG) : sig
  val env : Sqlpp.Env.t
  val register : unit -> unit
end

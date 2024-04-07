module Make (Db : Sqlpp.DB) : sig
  val env : Sqlpp.Env.t
  val register : unit -> unit
end

include ContainersLabels
include Printf

let failwithf fmt = ksprintf failwith fmt

type 'a ord = 'a Ord.t
type 'a eq = 'a Equal.t
type 'a hash = 'a Hash.t
type 'a hash_fold = 'a Ppx_hash_lib.hash_fold

(** equivalence class *)
module Eq_class : sig
  type !'a t

  val equal : 'a eq
  val hash : 'a t hash
  val hash_fold_t : 'a t hash_fold

  module Make (H : Hashtbl.HashedType) : sig
    val v : H.t -> H.t t
  end
end = struct
  type !'a t = { hash : int }

  let equal a b = Equal.physical a b
  let hash a = a.hash
  let hash_fold_t s a = Ppx_hash_lib.Std.Hash.fold_int s a.hash

  module Make (H : Hashtbl.HashedType) : sig
    val v : H.t -> H.t t
  end = struct
    module Items = Ephemeron.K1.Make (H)

    let items = Items.create 1023

    let v x =
      match Items.find items x with
      | exception Not_found ->
          let item = { hash = H.hash x } in
          Items.add items x item;
          item
      | eq -> eq
  end
end

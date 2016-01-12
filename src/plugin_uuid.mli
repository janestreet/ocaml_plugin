(* [t] is a uniq identifier for plugin, along with some information needed for code
   generation and that make debugging nicer because they end up in the cache info.
*)

open Core.Std

module Repr : sig
  type t
  val create : t:string -> univ_constr:string -> t
  val t : t -> string
  val univ_constr : t -> string
end

type t [@@deriving sexp]

val uuid : t -> Uuid.t

val create :
  repr:Repr.t
  -> ml_bundles:Ml_bundle.t list
  -> unit
  -> t

val ml_bundles : t -> Ml_bundle.t list
val repr : t -> Repr.t

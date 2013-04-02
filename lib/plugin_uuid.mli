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

type t

val uuid : t -> Uuid.t

include Sexpable             with type t := t
include Comparable.S_binable with type t := t
include Hashable.S_binable   with type t := t

val create :
  repr:Repr.t option
  -> ml_bundles:Ml_bundle.t list
  -> unit
  -> t

val ml_bundles : t -> Ml_bundle.t list
val repr : t -> Repr.t option

(*
  via Sexp.of_string,
  so that the generated plugin doesn't needlessly depends on sexplib.cmi
*)
val t_of_string : string -> t
val string_of_t : t -> string

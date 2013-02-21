open Core.Std

module type S = sig
  val message : string
end

let univ_constr =
  (Univ.Constr.create "Plugin_intf.S" sexp_of_opaque : (module S) Univ.Constr.t)


open Ocaml_plugin.Std

module type S = sig
  val message : string
end

let univ_constr : (module S) Ocaml_dynloader.Univ_constr.t =
  Ocaml_dynloader.Univ_constr.create ()

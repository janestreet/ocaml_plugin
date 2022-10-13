module type S = sig
  val message : string
end

let univ_constr : (module S) Ocaml_plugin.Dynloader.Univ_constr.t =
  Ocaml_plugin.Dynloader.Univ_constr.create ()
;;

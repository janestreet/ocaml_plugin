open Ocaml_plugin.Std

type v1 = (module Dsl.Config_intf_v1)
type v2 = (module Dsl.Config_intf_v2)

module V1 = Ocaml_compiler.Make(struct
    type t = v1
    let t_repr = "Dsl.Config_intf_v1"
    let univ_constr = Dsl.univ_constr_v1
    let univ_constr_repr = "Dsl.univ_constr_v1"
  end)

module V2 = Ocaml_compiler.Make(struct
    type t = v2
    let t_repr = "Dsl.Config_intf_v2"
    let univ_constr = Dsl.univ_constr_v2
    let univ_constr_repr = "Dsl.univ_constr_v2"
  end)

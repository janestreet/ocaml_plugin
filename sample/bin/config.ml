type v1 = (module Ocaml_plugin_sample.Dsl.Config_intf_v1)
type v2 = (module Ocaml_plugin_sample.Dsl.Config_intf_v2)

module V1 = Ocaml_plugin.Compiler.Make (struct
    type t = v1

    let t_repr = "Ocaml_plugin_sample.Dsl.Config_intf_v1"
    let univ_constr = Ocaml_plugin_sample.Dsl.univ_constr_v1
    let univ_constr_repr = "Ocaml_plugin_sample.Dsl.univ_constr_v1"
  end)

module V2 = Ocaml_plugin.Compiler.Make (struct
    type t = v2

    let t_repr = "Ocaml_plugin_sample.Dsl.Config_intf_v2"
    let univ_constr = Ocaml_plugin_sample.Dsl.univ_constr_v2
    let univ_constr_repr = "Ocaml_plugin_sample.Dsl.univ_constr_v2"
  end)

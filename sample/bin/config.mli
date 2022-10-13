type v1 = (module Ocaml_plugin_sample.Dsl.Config_intf_v1)
type v2 = (module Ocaml_plugin_sample.Dsl.Config_intf_v2)

module V1 : Ocaml_plugin.Compiler.S with type t := v1
module V2 : Ocaml_plugin.Compiler.S with type t := v2

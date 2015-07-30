open Ocaml_plugin.Std

type v1 = (module Dsl.Config_intf_v1)
type v2 = (module Dsl.Config_intf_v2)

module V1 : Ocaml_compiler.S with type t := v1
module V2 : Ocaml_compiler.S with type t := v2

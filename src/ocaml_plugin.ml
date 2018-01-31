module Dynloader      = Ocaml_dynloader
module Compiler       = Ocaml_compiler
module Plugin_cache   = Plugin_cache

module Private = struct
  module Shell = Shell
  module Tar = Tar
end

module Std = struct
  module Ocaml_dynloader    = Ocaml_dynloader
  module Ocaml_compiler     = Ocaml_compiler
  module Plugin_cache       = Plugin_cache
end
[@@deprecated "[since 2018-01] Use [Ocaml_plugin]. \
               The [Ocaml_plugin.Std] sub-module is no longer needed."]

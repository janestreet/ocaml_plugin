module Dynloader      = Dynloader
module Compiler       = Compiler
module Plugin_cache   = Plugin_cache

module Private = struct
  module Shell = Shell
  module Tar = Tar
end

module Std = struct
  module Ocaml_dynloader    = Dynloader
  module Ocaml_compiler     = Compiler
  module Plugin_cache       = Plugin_cache
end
[@@deprecated "[since 2018-01] Use [Ocaml_plugin]. \
               The [Ocaml_plugin.Std] sub-module is no longer needed."]

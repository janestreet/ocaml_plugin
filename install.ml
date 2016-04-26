#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"ocaml_plugin"
  [ oasis_lib "ocaml_plugin"
  ; oasis_lib "ocaml_plugin_ocamlbuild"
  ; file "META" ~section:"lib"
  ; oasis_exe "ocaml-embed-compiler" ~dest:"ocaml-embed-compiler"
  ]

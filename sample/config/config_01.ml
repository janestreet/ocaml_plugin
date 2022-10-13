(**
   This is a standalone config for main
*)

module Fancy = struct
  let x = "I can use fancy OCaml features in my config files"
end

(*
   We open the main Api to simulate DSL keywords
   make, register, etc...
*)
open Ocaml_plugin_sample.Dsl

(** in a sexp DSL, this would be:
    (job something)
*)

(* in OCaml, this gets written : *)
let job = make_v1 Fancy.x

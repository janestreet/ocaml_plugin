open! Core

(* example of plugin of type Plugin_intf.S *)

let side_effect = print_endline "plugin_001.ml is being executed !"
let message = "This is plugin_001's message"

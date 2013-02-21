open Core.Std let _ = _squelch_unused_module_warning_

(* example of plugin of type Plugin_intf.S *)

let side_effect = print_endline "plugin_001.ml is being executed !"

let message = "This is plugin_001's message"

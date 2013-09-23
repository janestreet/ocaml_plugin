open Core.Std let () = _squelch_unused_module_warning_
open Async.Std

(* some shared internal util for ocaml_plugin *)

let if_ cond fct = if cond then fct () else Deferred.Or_error.ok_unit

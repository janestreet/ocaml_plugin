open Core.Std
open Async.Std
open Ocaml_plugin.Std

let run files =
  Ocaml_compiler.load_ocaml_src_files files >>= function
  | Ok () -> return (Shutdown.shutdown 0)
  | Error e -> Error.raise e

let main =
  match Array.to_list Sys.argv with
  | [] -> failwith "wth"
  | _ :: files -> run files

let () =
  Exn.handle_uncaught ~exit:true (fun () -> never_returns (Scheduler.go ()))

open Core.Std
open Async.Std
open Ocaml_plugin.Std

let run ?use_cache files =
  Sys.getcwd () >>= fun cwd ->
  let in_dir = Filename.concat cwd "tmp_dir" in
  Ocaml_compiler.load_ocaml_src_files ~in_dir ?use_cache files >>= function
  | Ok () -> Deferred.unit
  | Error e -> Error.raise e

let groups l =
  List.map (String.split ~on:'|' (String.concat l ~sep:" ")) ~f:(fun s ->
    List.filter (String.split ~on:' ' s) ~f:(function
    | "" -> false
    | _ -> true
  ))

let use_cache =
  Plugin_cache.Config.create
    ~dir:"cache"
    ~max_files:2
    ~readonly:false
    ~try_old_cache_with_new_exec:true
    ()

let main =
  match Array.to_list Sys.argv with
  | [] -> failwith "wth"
  | _ :: "cache" :: files ->
    Deferred.List.iter ~how:`Sequential (groups files) ~f:(run ~use_cache) >>= fun () ->
    return (Shutdown.shutdown 0)
  | _ :: files ->
    Deferred.List.iter ~how:`Sequential (groups files) ~f:run >>= fun () ->
    return (Shutdown.shutdown 0)

let () =
  Exn.handle_uncaught ~exit:true (fun () -> never_returns (Scheduler.go ()))

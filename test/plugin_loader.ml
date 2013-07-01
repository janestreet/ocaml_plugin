open Core.Std
open Async.Std
open Ocaml_plugin.Std

let run
    ?use_cache
    ~trigger_unused_value_warnings_despite_mli
    files =
  Sys.getcwd () >>= fun cwd ->
  let in_dir = Filename.concat cwd "tmp_dir" in
  Ocaml_compiler.load_ocaml_src_files
    ~in_dir
    ~trigger_unused_value_warnings_despite_mli
    ?use_cache
    files >>= function
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

module Flags = struct
  open Command.Spec
  let use_cache () =
    map ~f:(fun b -> if b then Some use_cache else None)
      (flag "--cache" no_arg ~doc:" use a plugin cache")

  let trigger_unused_value_warnings_despite_mli () =
    flag "--warnings-in-utils" no_arg
      ~doc:" trigger unused warnings even in utils with an mli"

  let anon_files () =
    anon (sequence ("<ocaml-file>" %: file))
end

let summary = "unit test program for ocaml-plugin"

let command =
  Command.(async_basic ~summary Spec.(
    empty
    +> Flags.use_cache ()
    +> Flags.trigger_unused_value_warnings_despite_mli ()
    +> Flags.anon_files ()
  )) (fun
    use_cache
    trigger_unused_value_warnings_despite_mli
    files
    ()
  ->
    Deferred.List.iter ~how:`Sequential (groups files) ~f:(
      run
        ?use_cache
        ~trigger_unused_value_warnings_despite_mli
    ) >>= fun () ->
    return (Shutdown.shutdown 0)
  )

let () =
  Exn.handle_uncaught ~exit:true (fun () -> Command.run command)

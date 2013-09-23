open Core.Std
open Async.Std
open Ocaml_plugin.Std

let run
    ?use_cache
    ?persistent_archive_dirpath
    ~trigger_unused_value_warnings_despite_mli
    ~run_plugin_toplevel
    files =
  Sys.getcwd () >>= fun cwd ->
  let in_dir = Filename.concat cwd "tmp_dir" in
  Ocaml_compiler.load_ocaml_src_files
    ~in_dir
    ~trigger_unused_value_warnings_despite_mli
    ~run_plugin_toplevel
    ?use_cache
    ?persistent_archive_dirpath
    files >>= function
  | Ok () -> Deferred.unit
  | Error e ->
    prerr_endline (Error.to_string_hum e);
    return (Shutdown.shutdown 1)
;;

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

  let persistent_archive () =
    map ~f:(fun b -> if b then Some "cache" else None)
      (flag "--persistent-archive" no_arg
         ~doc:" use a persistent location for the extracted compiler")

  let trigger_unused_value_warnings_despite_mli () =
    flag "--warnings-in-utils" no_arg
      ~doc:" trigger unused warnings even in utils with an mli"

  let run_plugin_toplevel () =
    map ~f:(fun b -> if b then `Outside_of_async else `In_async_thread)
      (flag "--toplevel-outside-of-async" no_arg
         ~doc:" execute plugin's toplevel outside of async")

  let anon_files () =
    anon (sequence ("<ocaml-file>" %: file))
end

let summary = "unit test program for ocaml-plugin"

let command =
  Command.(async_basic ~summary Spec.(
    empty
    +> Flags.use_cache ()
    +> Flags.persistent_archive ()
    +> Flags.trigger_unused_value_warnings_despite_mli ()
    +> Flags.run_plugin_toplevel ()
    +> Flags.anon_files ()
  )) (fun
    use_cache
    persistent_archive_dirpath
    trigger_unused_value_warnings_despite_mli
    run_plugin_toplevel
    files
    ()
  ->
    Deferred.List.iter ~how:`Sequential (groups files) ~f:(
      run
        ?use_cache
        ?persistent_archive_dirpath
        ~trigger_unused_value_warnings_despite_mli
        ~run_plugin_toplevel
    ) >>= fun () ->
    return (Shutdown.shutdown 0)
  )

let () =
  Exn.handle_uncaught ~exit:true (fun () -> Command.run command)

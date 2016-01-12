open Core.Std
open Async.Std
open Ocaml_plugin.Std

let run
      ?use_cache
      ?persistent_archive_dirpath
      ~trigger_unused_value_warnings_despite_mli
      ~ppx_flag
      ~run_plugin_toplevel
      ~find_dependencies
      files =
  Sys.getcwd () >>= fun cwd ->
  let in_dir = Filename.concat cwd "tmp_dir" in
  let code_style = if ppx_flag then `Ppx_style else `Camlp4_style in
  (if find_dependencies
   then match files with
     | [file] ->
       Ocaml_compiler.with_compiler
         ~in_dir
         ~trigger_unused_value_warnings_despite_mli
         ~run_plugin_toplevel
         ~code_style
         ?use_cache
         ?persistent_archive_dirpath
         ~f:(fun compiler ->
           let loader = Ocaml_compiler.loader compiler in
           Ocaml_dynloader.find_dependencies loader file >>=? fun files ->
           begin match Sys.getenv "VERBOSE" with
           | None -> ()
           | Some _ ->
             Core.Std.Printf.printf "Loaded %s\n"
               (String.concat (List.map files ~f:Filename.basename) ~sep:" ")
           end;
           Ocaml_dynloader.Side_effect.load_ocaml_src_files loader files
         )
         ()
     | _ ->
       failwithf "When --find-dependencies is specified, only one file should be given" ()
   else
     Ocaml_compiler.Side_effect.load_ocaml_src_files
       ~in_dir
       ~trigger_unused_value_warnings_despite_mli
       ~run_plugin_toplevel
       ~code_style
       ?use_cache
       ?persistent_archive_dirpath
       files)
  >>= function
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

let max_files_default = 2

let use_cache ~max_files =
  Plugin_cache.Config.create
    ~dir:"cache"
    ~max_files
    ~readonly:false
    ~try_old_cache_with_new_exec:true
    ()

module Flags = struct
  open Command.Spec
  let use_cache () =
    map ~f:(fun b -> if b then Some use_cache else None)
      (flag "--cache" no_arg ~doc:" use a plugin cache")

  let cache_size () =
    flag "--cache-size" (optional_with_default max_files_default int)
      ~doc:(sprintf " specify size of plugin cache. default %d" max_files_default)

  let persistent_archive () =
    map ~f:(fun b -> if b then Some "cache" else None)
      (flag "--persistent-archive" no_arg
         ~doc:" use a persistent location for the extracted compiler")

  let trigger_unused_value_warnings_despite_mli () =
    flag "--warnings-in-utils" no_arg
      ~doc:" trigger unused warnings even in utils with an mli"

  let ppx_flag () =
    flag "--ppx" no_arg
      ~doc:" expect ocaml files to be in ppx-style"

  let run_plugin_toplevel () =
    map ~f:(fun b -> if b then `Outside_of_async else `In_async_thread)
      (flag "--toplevel-outside-of-async" no_arg
         ~doc:" execute plugin's toplevel outside of async")

  let find_dependencies () =
    flag "--find-dependencies" no_arg
      ~doc:" use ocamldep to generate dependencies"

  let anon_files () =
    anon (sequence ("<ocaml-file>" %: file))
end

let command =
  Command.async ~summary:"unit test program for ocaml-plugin"
    Command.Spec.(
      empty
      ++ step (fun m use_cache cache_size ->
        m (Option.map use_cache ~f:(fun f -> f ~max_files:cache_size)))
      +> Flags.use_cache ()
      +> Flags.cache_size ()
      +> Flags.persistent_archive ()
      +> Flags.trigger_unused_value_warnings_despite_mli ()
      +> Flags.ppx_flag ()
      +> Flags.run_plugin_toplevel ()
      +> Flags.find_dependencies ()
      +> Flags.anon_files ()
    ) (fun
      use_cache
      persistent_archive_dirpath
      trigger_unused_value_warnings_despite_mli
      ppx_flag
      run_plugin_toplevel
      find_dependencies
      files
      ()
    ->
      Deferred.List.iter ~how:`Sequential (groups files) ~f:(
        run
          ?use_cache
          ?persistent_archive_dirpath
          ~trigger_unused_value_warnings_despite_mli
          ~ppx_flag
          ~run_plugin_toplevel
          ~find_dependencies
      ) >>= fun () ->
      return (Shutdown.shutdown 0)
    )

let () =
  Exn.handle_uncaught ~exit:true (fun () -> Command.run command)

open Core.Std
open Async.Std
open Ocaml_plugin.Std

let run
      ?use_cache
      ?persistent_archive_dirpath
      ~trigger_unused_value_warnings_despite_mli
      ~run_plugin_toplevel
      ~find_dependencies
      files =
  Sys.getcwd () >>= fun cwd ->
  let in_dir = Filename.concat cwd "tmp_dir" in
  (if find_dependencies
   then match files with
     | [file] ->
       Ocaml_compiler.with_compiler
         ~in_dir
         ~trigger_unused_value_warnings_despite_mli
         ~run_plugin_toplevel
         ?use_cache
         ?persistent_archive_dirpath
         ~f:(fun compiler ->
           let loader = Ocaml_compiler.loader compiler in
           Ocaml_dynloader.find_dependencies loader file >>=? fun files ->
           begin match Sys.getenv "VERBOSE" with
           | None -> ()
           | Some _ ->
             Printf.printf "Loaded %s\n"
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
      +> Flags.use_cache ()
      +> Flags.persistent_archive ()
      +> Flags.trigger_unused_value_warnings_despite_mli ()
      +> Flags.run_plugin_toplevel ()
      +> Flags.find_dependencies ()
      +> Flags.anon_files ()
    ) (fun
      use_cache
      persistent_archive_dirpath
      trigger_unused_value_warnings_despite_mli
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
          ~run_plugin_toplevel
          ~find_dependencies
      ) >>= fun () ->
      return (Shutdown.shutdown 0)
    )

let () =
  Exn.handle_uncaught ~exit:true (fun () -> Command.run command)

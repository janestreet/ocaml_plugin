open Core
open Async
open Ocaml_plugin.Std

let run
      ?use_cache
      ?persistent_archive_dirpath
      ~trigger_unused_value_warnings_despite_mli
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
         ?use_cache
         ?persistent_archive_dirpath
         ~f:(fun compiler ->
           let loader = Ocaml_compiler.loader compiler in
           Ocaml_dynloader.find_dependencies loader file >>=? fun files ->
           begin match Sys.getenv "VERBOSE" with
           | None -> ()
           | Some _ ->
             Core.Printf.printf "Loaded %s\n"
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
      | _ -> true))
;;

let max_files_default = 2
;;

let use_cache ~max_files =
  Plugin_cache.Config.create
    ~dir:"cache"
    ~max_files
    ~readonly:false
    ~try_old_cache_with_new_exec:true
    ()
;;

let command =
  Command.async ~summary:"unit test program for ocaml-plugin"
    (let open Command.Let_syntax in
     let%map_open use_cache =
       let%map cache = flag "--cache" no_arg ~doc:" use a plugin cache"
       and cache_size =
         flag "--cache-size" (optional_with_default max_files_default int)
           ~doc:(sprintf " specify size of plugin cache. default %d" max_files_default)
       in
       if cache then Some (use_cache ~max_files:cache_size)
       else None
     and persistent_archive_dirpath =
       map ~f:(fun b -> if b then Some "cache" else None)
         (flag "--persistent-archive" no_arg
            ~doc:" use a persistent location for the extracted compiler")
     and trigger_unused_value_warnings_despite_mli =
       flag "--warnings-in-utils" no_arg
         ~doc:" trigger unused warnings even in utils with an mli"
     and find_dependencies =
       flag "--find-dependencies" no_arg
         ~doc:" use ocamldep to generate dependencies"
     and files =
       anon (sequence ("<ocaml-file>" %: file))
     in
     fun () ->
       let open! Deferred.Let_syntax in
       Deferred.List.iter ~how:`Sequential (groups files) ~f:(
         run
           ?use_cache
           ?persistent_archive_dirpath
           ~trigger_unused_value_warnings_despite_mli
           ~find_dependencies))
;;

let () = Command.run command

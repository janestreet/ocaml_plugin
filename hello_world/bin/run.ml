(* simplest example for using ocaml_plugin *)
(*
   This is an interactive loop. It is meant to be used for small demo, testing the cache,
   looking at the build dir while the application is still running, experimenting various
   failure scenarios, etc.
   run with:
   rlwrap ./run.exe
   enter ml filename(s) to load: plugin_001.ml
*)

open Core
open Async

module Plugin = Ocaml_plugin.Dynloader.Make(struct
    type t = (module Ocaml_plugin_hello_world.Plugin_intf.S)
    let t_repr = "Ocaml_plugin_hello_world.Plugin_intf.S"
    let univ_constr = Ocaml_plugin_hello_world.Plugin_intf.univ_constr
    let univ_constr_repr = "Ocaml_plugin_hello_world.Plugin_intf.univ_constr"
  end)

let () =
  don't_wait_for (
    Ocaml_plugin.Private.Shell.set_defaults ~verbose:true ~echo:true ();
    let use_cache =
      if Array.length Sys.argv > 1 then
        Some (Ocaml_plugin.Plugin_cache.Config.t_of_sexp (Sexp.of_string (Sys.argv.(1))))
      else
        None
    in
    let persistent_archive_dirpath =
      if Array.length Sys.argv > 2 then
        Some Sys.argv.(2)
      else None
    in
    Ocaml_plugin.Compiler.create
      ?use_cache
      ?persistent_archive_dirpath
      () >>= function
    | Error e ->
      Core.Printf.eprintf "Cannot build embed loader: %s" (Error.to_string_hum e);
      Core.Printf.eprintf "use run_standalone.exe (cf build.sh) instead\n%!";
      exit 1
    | Ok (`this_needs_manual_cleaning_after compiler) ->
      let loader = Ocaml_plugin.Compiler.loader compiler in
      let stdin = Lazy.force Reader.stdin in
      let rec loop () =
        Core.Printf.printf "enter ml filename(s) to load: %!";
        Reader.read_line stdin >>= function
        | `Eof ->
          Ocaml_plugin.Compiler.clean compiler >>= fun result ->
          let () = Or_error.ok_exn result in
          print_newline ();
          return (shutdown 0)
        | `Ok input ->
          let files = String.split ~on:' ' input in
          let files = List.filter_map files
                        ~f:(fun s -> let s = String.strip s in if s = "" then None else Some s)
          in
          let with_files files =
            Plugin.load_ocaml_src_files loader files >>= function
            | Error err ->
              Core.Printf.eprintf "loading failed:\n%s\n%!" (Error.to_string_hum err);
              loop ()
            | Ok plugin ->
              let module M = (val plugin : Ocaml_plugin_hello_world.Plugin_intf.S) in
              Core.Printf.printf "loaded plugin's message : %S\n%!" M.message;
              loop ()
          in
          match files with
          | [] -> loop ()
          | [ cmxs ] when String.is_suffix ~suffix:"cmxs" cmxs ->
            (* hack to play a bit with ocaml dynlink *)
            (try Dynlink.loadfile_private cmxs
             with
             | e ->
               let str =
                 match e with
                 | Dynlink.Error e -> Dynlink.error_message e
                 | e -> Exn.to_string e
               in
               Core.Printf.eprintf "dynlink failed:\n%s\n%!" str
            );
            loop ()
          | [ "dep" ; file ] -> begin
              (* hack to play a bit with ocamldep *)
              Ocaml_plugin.Dynloader.find_dependencies loader file >>= function
              | Error err ->
                Core.Printf.eprintf "ocamldep failed:\n%s\n%!" (Error.to_string_hum err);
                loop ()
              | Ok files -> with_files files
            end

          | _ -> with_files files
      in
      loop ()
  )

let () = never_returns (Scheduler.go ())

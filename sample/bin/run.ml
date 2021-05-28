open! Core
open! Async

(*
   For that sample, we will assume that the command ocamlopt.opt
   refer to the same executable used to compile run.exe
   This sample does not use the binutils packaging, it only
   tests the Loader module.
*)
let ocamlopt_opt = "ocamlopt.opt"

(*
   We use a local build dir not in tmp so that we can have a look
   there and see what is generated and compiled (debugging)
   In production, this directory would be somewhere hidden in /tmp
   and cleaned right after the end of the dynloading.
*)
let builddir = Filename.concat (Core.Sys.getcwd()) "_build"

(*
   For building this example, we use 3 config files
   -config_01.ml is a version 1 of the Dsl's config
   -config_02.ml is a version 2 of the Dsl's config
   -config_util.ml is a utility file used by config_02
   it is used to show how to integrate library-like config scripts (cf ~export:true)
   since config_02.ml uses features from config_util, it should be loaded before.
*)
let conf_01   = "config/config_01.ml"
let conf_util = "config/config_util.ml"
let conf_02   = "config/config_02.ml"

(**
   The location where the cmi files needed by the plugin can be found.
   Any config loading need to access ocaml_plugin.cmi.
*)
let def_include = [ "." ; "../lib" ]

let include_directories = ref []

let cmx_flags = ref []
let cmxs_flags = ref []

(*
   use rather a temp_dir instead of the builddir
*)
let temp_dir = ref false

(*
   We use a flag to tell if we want to load the default files
*)
let default = ref true

(*
   We use a flag to use the auto embed build mode
*)
let embed_mode = ref false

(*
   we use a flag to test the clean function
*)
let clean = ref false

(*
   this file is used to show what happens in case of error, e.g. typing error
*)
let conf_v1_error = "config/config_v1_error.ml"
let load_error = ref false

(*
   taking file to load from command line
*)
type file = [`v1 of string | `v2 of string | `util of string]

let files = ( Queue.create () : file Queue.t )

module Flag = struct
  let string tag ~doc fct () =
    Command.Spec.(step (fun main strings ->
      List.iter ~f:fct strings;
      main) +> flag tag (listed string) ~doc)
  ;;

  let set tag ~doc ref_ () =
    Command.Spec.(step (fun main bool ->
      if bool then ref_ := true;
      main) +> flag tag no_arg ~doc)
  ;;

  let clear tag ~doc ref_ () =
    Command.Spec.(step (fun main bool ->
      if bool then ref_ := false;
      main) +> flag tag no_arg ~doc)
  ;;

  let noarg tag ~doc fct () =
    Command.Spec.(step (fun main bool ->
      if bool then fct ();
      main) +> flag tag no_arg ~doc)
  ;;
end

module Flags =
struct
  let split_files string =
    let files = String.split ~on:' ' string in
    List.map files ~f:String.strip
  ;;

  let add_file tag version =
    let add sources =
      let sources =
        match version with
        | `v1 -> `v1 sources
        | `v2 -> `v2 sources
        | `util -> `util sources
      in
      Queue.enqueue files sources
    in
    Flag.string
      tag
      ~doc:" add one or several ocaml config files to be loaded (sep by ' ')"
      add
  ;;

  let rev_append_flags rev_acc flags =
    let flags = String.split ~on:' ' flags in
    let fold acc flag = if String.is_empty flag then acc else flag :: acc in
    rev_acc := List.fold ~f:fold ~init:!rev_acc flags
  ;;

  let v1 = add_file "-v1" `v1
  let v2 = add_file "-v2" `v2
  let util = add_file "-f" `util
  ;;

  let embed =
    Flag.set
      "-embed"
      ~doc:" use the embedded mode of ocaml_plugin (needs standalone exe though)"
      embed_mode
  ;;

  let error =
    Flag.set
      "-error"
      ~doc:(Printf.sprintf " load an ill-typed config to see what happens %s" conf_v1_error)
      load_error
  ;;

  let clean =
    Flag.set
      "-clean"
      ~doc:" clean the builddir after dynamic loading"
      clean
  ;;

  let no_default =
    Flag.clear
      "-n"
      ~doc:" do not load the default conf files, use custom files only"
      default
  ;;

  let temp_dir =
    Flag.set
      "-tmp"
      ~doc:(Printf.sprintf " use a temp dir instead of %s" builddir)
      temp_dir
  ;;

  let include_dir =
    Flag.string
      "-I"
      ~doc:"<dir>  Add <dir> to the list of include directories"
      (fun dir -> include_directories := dir :: !include_directories)
  ;;

  let cmx_flag =
    Flag.string
      "-cmx-f"
      ~doc:"<flag> Add flag to the ocamlopt compilation of the cmx"
      (rev_append_flags cmx_flags)
  ;;

  let cmxs_flag =
    Flag.string
      "-cmxs-f"
      ~doc:"<flag> Add flag to the ocamlopt -shared compilation of the cmxs"
      (rev_append_flags cmxs_flags)
  ;;

  let all () =
    Command.Spec.(step (fun main -> main ())
                  ++ v1 ()
                  ++ v2 ()
                  ++ util ()
                  ++ embed ()
                  ++ error ()
                  ++ clean ()
                  ++ no_default ()
                  ++ include_dir ()
                  ++ temp_dir ()
                  ++ cmx_flag ()
                  ++ cmxs_flag ()
                  ++ Command.Spec.step (fun k () -> k)
                  +> Ocaml_plugin.Private.Shell.flags
                 )
  ;;
end

let readme () = "\
This toy exe is intended to:
 * test the ocaml_plugin library
 * show a code sample for potential users of the library

You can try to load some custom ocaml source files,
play a bit with the options to test and see what happens
in case of type errors, etc.
"
;;

let summary = "simple example to test the ocaml_plugin library"
;;

let handle_error load = function
  | Result.Ok m -> load m
  | Result.Error e ->
    Core.Printf.eprintf "Error:\n%s\n%!" (Error.to_string_hum e)
;;

let load_file loader = function
  | `v1 files ->
    let v1 = Config.V1.Load.load_ocaml_src_files loader (Flags.split_files files) in
    v1 >>| (handle_error Ocaml_plugin_sample.Dsl.register_v1)
  | `v2 files ->
    let v2 = Config.V2.Load.load_ocaml_src_files loader (Flags.split_files files) in
    v2 >>| (handle_error Ocaml_plugin_sample.Dsl.register_v2)
  | `util files ->
    let res =
      Ocaml_plugin.Dynloader.Side_effect.load_ocaml_src_files loader (Flags.split_files files)
    in
    res >>| (handle_error ignore)
;;

let load_default loader =
  load_file loader (`v1 conf_01) >>= fun () ->
  let v2 = Config.V2.Load.load_ocaml_src_files loader [conf_util;conf_02] in
  v2 >>| (handle_error Ocaml_plugin_sample.Dsl.register_v2)
;;

let main () () =
  let k_clean_loader =
    let compilation_directory =
      if !temp_dir then None
      else Some builddir
    in
    let include_directories =
      let directories = List.rev !include_directories in
      if !default && not !embed_mode
      then def_include @ directories
      else directories
    in
    let cmx_flags = List.rev !cmx_flags in
    let cmxs_flags = List.rev !cmxs_flags in
    if !embed_mode
    then
      let build =
        Ocaml_plugin.Compiler.create
          ~include_directories
          ~cmx_flags
          ~cmxs_flags
          ()
      in
      build >>= function
      | Error e ->
        Core.Printf.eprintf "Cannot build embed loader: %s" (Error.to_string_hum e);
        exit 1
      | Ok (`this_needs_manual_cleaning_after compiler) ->
        return
          ((fun () -> Ocaml_plugin.Compiler.clean compiler),
           Ocaml_plugin.Compiler.loader compiler)
    else
      Ocaml_plugin.Dynloader.create
        ?in_dir:compilation_directory
        ~cmx_flags
        ~cmxs_flags
        ~include_directories
        ~ocamlopt_opt
        ()
      >>| function
      | Error e -> Error.raise e
      | Ok loader ->
        ((fun () -> Ocaml_plugin.Dynloader.clean loader),
         loader)
  in
  let dt =
    k_clean_loader >>= fun (k_clean, loader) ->
    (if !default then load_default loader else Deferred.return ())
    >>= fun () ->
    (if !load_error then load_file loader (`v1 conf_v1_error) else Deferred.return ())
    >>= fun () ->
    (Deferred.Queue.iter ~f:(load_file loader) files)
    >>| Ocaml_plugin_sample.Dsl.exec
    >>= fun () ->
    (if !clean then k_clean () else Deferred.return (Ok ())) >>| function
    | Error e -> Error.raise e
    | Ok () -> ()
  in
  dt >>> (fun () -> shutdown 0);
  Deferred.never ()
;;

let run_command =
  Command.async_spec ~summary ~readme (
    Flags.all ()
  ) main
;;

let command =
  Command.group ~summary:"Toy sample program for ocaml_plugin" [
    "run", run_command;
    "check-plugin-v1", Config.V1.check_plugin_cmd ();
    "check-plugin-v2", Config.V2.check_plugin_cmd ();
  ]
;;

let () =
  Exn.handle_uncaught ~exit:true (fun () -> Command_unix.run command)
;;

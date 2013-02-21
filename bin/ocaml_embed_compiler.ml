open Core.Std
open Async.Std
open Ocaml_plugin.Std

let verbose = ref false
let target = ref None
let ocamlopt_opt = ref None
let camlp4o_opt = ref None
let pa_files = ref []
let exe = ref None
let extract = ref None

let (>>=!) a fct = a >>= fun result -> fct (Or_error.ok_exn result)
let (>>|!) a fct = a >>| fun result -> fct (Or_error.ok_exn result)

module Flags =
struct

  module Spec = struct
    let set tag ~doc ref_ () =
      Command.Spec.(step (fun main bool ->
        if bool then ref_ := true;
        main) +> flag tag no_arg ~doc)

    let set_string_opt tag ~doc ref_ () =
      Command.Spec.(step (fun main str ->
        ref_ := str;
        main) +> flag tag (optional string) ~doc)

    let string tag ~doc fct () =
      Command.Spec.(step (fun main strings ->
        List.iter ~f:fct strings;
        main) +> flag tag (listed string) ~doc)
  end

  let exe =
    Spec.set_string_opt
      "-exe"
      ~doc:"</path/to/prog.exe> set the name of the initial executable"
      exe

  let extract =
    Spec.set_string_opt
      "-x"
      ~doc:"</path/to/prog-standalone.exe> extract the embedded section of the executable"
      extract

  let ocamlopt_opt =
    Spec.set_string_opt
      "-cc"
      ~doc:"</path/to/ocamlopt.opt> set the ocaml native compiler"
      ocamlopt_opt

  let camlp4o_opt =
    Spec.set_string_opt
      "-pp"
      ~doc:"</path/to/camlp4o.opt> set the camlp4o native pre-processor"
      camlp4o_opt

  let pa_files =
    Spec.string
      "-pa-cmxs"
      ~doc:" path to a native syntax extension plugin file (.cmxs)"
      (fun file -> pa_files := file :: !pa_files)

  let target =
    Spec.set_string_opt
      "-o"
      ~doc:"<prog.exe> set the name of the resulting executable to be created"
      target

  let verbose =
    Spec.set
      "-verbose"
      ~doc:" be more verbose"
      verbose

  let files () =
    Command.Spec.(anon (sequence ("<embedded-file>*" %: file)))

  let all () = Command.Spec.(step (fun main files -> main files)
    ++ exe ()
    ++ extract ()
    ++ ocamlopt_opt ()
    ++ camlp4o_opt ()
    ++ pa_files ()
    ++ target ()
    ++ verbose ()
    +> files ()
  )
end

let readme () = "\
This command is aimed to produce a standalone native executable
from an existing executable by embedding ocamlopt and some cmi
files into it so that the resulting executable may use ocaml
source file as ocaml plugins with the ocaml_plugin library.

You should use this command to produce your executable if it
uses the Ocaml_plugin.Ocaml_compiler module.
"

let summary =
  "tool to embed ocamlopt and cmi files into a standalone exe using ocaml_plugin"

let usage_arg = "-o <exe> -cc </path/to/ocamlopt.opt> -exe </path/to/exe> embedded-files*"

let get name opt =
  match !opt with
  | Some data -> data
  | None ->
    failwithf "%s is unspecified" name ()

exception Suspicious_files_in_tar of string list * string list with sexp
exception Missing_files_in_tar of string list * string list with sexp

let tar_check files tar =
  let cmp = String.compare in
  let files = List.sort ~cmp files in
  let tar = List.sort ~cmp tar in
  if not (List.equal files tar ~equal:String.equal)
  then (
    let mem l a = List.mem l a ~equal:String.equal in
    let missing = List.filter ~f:(mem tar) files in
    let unknown = List.filter ~f:(mem files) tar in
    if unknown <> []
    then raise (Suspicious_files_in_tar (unknown, files));
    if missing <> []
    then raise (Missing_files_in_tar (missing, files));
  )

let main files () =
  let _set_defaults_scope =
    let verbose = !verbose in
    let echo = verbose in
    Ocaml_plugin.Shell.set_defaults ~verbose ~echo ();
  in
  match !extract with
  | Some filename -> (
    let iter s =
      Printf.eprintf "extract: don't know what do to with argument: %S" s;
      shutdown 1
    in
    List.iter ~f:iter files;
    Ocaml_compiler.create ~code_file:(`file filename) () >>= function
    | Error e -> Error.raise e
    | Ok (`this_needs_manual_cleaning_after compiler) -> begin
      Ocaml_compiler.directory_files compiler >>| function
      | Ok (directory, files) ->
        Printf.printf "embedded contents extracted in: %s\n" directory;
        Printf.printf "files:\n";
        List.iter ~f:(fun file -> Printf.printf "  %s\n" file) files;
        Printf.printf "dont forget to clean the directory once you're done, using:\n";
        Printf.printf "  rm -rf %s\n" directory;
        shutdown 0
      | Error e -> Error.raise e
    end
  )
  | None ->
    let target = get "-o" target in
    let ocamlopt_opt = get "-cc" ocamlopt_opt in
    let exe = get "-exe" exe in
    Ocaml_plugin.Shell.temp_dir ~in_dir:Filename.temp_dir_name >>=! fun tmpdir ->
    let seen = String.Table.create () in
    let cp ~filename ~basename =
      let () =
        match String.Table.add seen ~key:basename ~data:filename with
        | `Ok -> ()
        | `Duplicate ->
          failwithf "basename collision: %s" basename ()
      in
      Ocaml_plugin.Shell.cp ~source:filename ~dest:(tmpdir ^/ basename) >>|! fun () ->
      ()
    in
    let copy_file filename =
      let basename = Filename.basename filename in
      cp ~filename ~basename >>| fun () ->
      basename
    in
    Deferred.List.map ~how:`Parallel files ~f:copy_file >>= fun files ->
    cp ~filename:ocamlopt_opt ~basename:Ocaml_dynloader.ocamlopt_opt >>= fun () ->
    let files = Ocaml_dynloader.ocamlopt_opt :: files in
    (
      match !camlp4o_opt with
      | None ->
        begin match !pa_files with
        | [] -> Deferred.return []
        | _ -> failwith "syntax extension files given, but the -camlp4 flag is absent"
        end
      | Some camlp4o_opt ->
        Deferred.List.map ~how:`Parallel (List.rev !pa_files) ~f:copy_file
        >>= fun pa_files ->
        let config = { Ocaml_dynloader.Config. pa_files } in
        let config_file = tmpdir ^/ Ocaml_compiler.config_file in
        Sexp.save_hum config_file (Ocaml_dynloader.Config.sexp_of_t config) ;
        cp ~filename:camlp4o_opt ~basename:Ocaml_dynloader.camlp4o_opt >>| fun () ->
        Ocaml_dynloader.camlp4o_opt :: Ocaml_compiler.config_file :: pa_files
    ) >>= fun camlp4_files ->
    let files = camlp4_files @ files in
    let tar = "a.tgz" in
    Ocaml_plugin.Tar.create ~working_dir:tmpdir ~files tar >>=! fun () ->
    let tar = tmpdir ^/ tar in
    Ocaml_plugin.Objcopy.embed
      ~filename:exe
      ~section_id:Ocaml_compiler.section_id
      ~section:tar
      ~destination:target
    >>=! fun () ->
      Ocaml_plugin.Tar.list tar >>=! fun check_files ->
      tar_check check_files files;
      Ocaml_plugin.Shell.rm ~r:() ~f:() [ tmpdir ] >>|! fun () -> ()

let command =
  Command.async_basic ~summary ~readme (
    Flags.all ()
  ) main

let () =
  Exn.handle_uncaught ~exit:true (fun () -> Command.run command)

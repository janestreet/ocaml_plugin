open Core.Std
open Async.Std
open Ocaml_plugin.Std

let verbose = ref false
let target = ref None
let ocamlopt_opt = ref None
let camlp4o_opt = ref None
let pa_files = ref []

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
      ~doc:"<file.c> set the name of the c file to be created"
      target

  let verbose =
    Spec.set
      "-verbose"
      ~doc:" be more verbose"
      verbose

  let files () =
    Command.Spec.(anon (sequence ("<embedded-file>*" %: file)))

  let all () = Command.Spec.(step (fun main files -> main files)
    ++ ocamlopt_opt ()
    ++ camlp4o_opt ()
    ++ pa_files ()
    ++ target ()
    ++ verbose ()
    +> files ()
  )
end

let readme () = "\
This tool archives ocamlopt, cmis, camlp4 and preprocessors in a c file containing a big
static string. The resulting .o file should be linked with any executable that uses
Ocaml_plugin.Ocaml_compiler module. Or you can link your executable with a .o file
containing a dummy definition of the function ocaml_plugin_archive if you know you will
not need it."

let summary =
  "tool to embed ocamlopt and cmi files into a c file"

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

let generate_c_file target tar =
  Monitor.try_with ~here:_here_ (fun () ->
    Reader.file_contents tar >>= fun str ->
    Writer.open_file target >>= fun w ->
    Writer.write w (String.concat ~sep:"\n" [
      "#include <string.h>";
      "#include <caml/mlvalues.h>";
      "#include <caml/memory.h>";
      "#include <caml/alloc.h>";
      "";
      "static unsigned char s[] = { ";
    ]);
    for i = 0 to String.length str - 1 do
      let c = str.[i] in
      let n = Char.to_int c in
      let column = i mod 16 in
      Writer.write w (if column = 0 then "\n  " else "");
      Writer.write w (string_of_int n);
      Writer.write w ",";
    done;
    if String.length str mod 16 <> 0 then Writer.write w "\n";
    Writer.write w (String.concat ~sep:"\n" [
      "};";
      "";
      "CAMLprim value ocaml_plugin_archive (value unit __attribute__ ((unused)))";
      "{";
      sprintf "  value v = caml_alloc_string(%d);" (String.length str);
      sprintf "  memcpy(String_val(v), s, %d);" (String.length str);
      "  return(v);";
      "}"
    ]);
    Writer.close w
  )

let main files () =
  let _set_defaults_scope =
    let verbose = !verbose in
    let echo = verbose in
    Ocaml_plugin.Shell.set_defaults ~verbose ~echo ();
  in
  let target = get "-o" target in
  let ocamlopt_opt = get "-cc" ocamlopt_opt in
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
  let copy_file =
    let fd_throttle =
      Throttle.create
        ~continue_on_error:true
        ~max_concurrent_jobs:30
    in
    fun filename ->
      Throttle.enqueue fd_throttle (fun () ->
        let basename = Filename.basename filename in
        cp ~filename ~basename >>| fun () ->
        basename
      )
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
  generate_c_file target tar
  >>= function
  | Error exn ->
    Ocaml_plugin.Shell.rm ~r:() ~f:() [ tmpdir ]
    >>|! fun () ->
    raise exn
  | Ok () ->
    Ocaml_plugin.Tar.list tar >>=! fun check_files ->
    tar_check check_files files;
    Ocaml_plugin.Shell.rm ~r:() ~f:() [ tmpdir ] >>|! fun () -> ()

let command =
  Command.async_basic ~summary ~readme (
    Flags.all ()
  ) main

let () =
  Exn.handle_uncaught ~exit:true (fun () -> Command.run command)

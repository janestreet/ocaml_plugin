open Core.Std
open Async.Std
open Ocaml_plugin.Std

let (>>=!) a fct = a >>= fun result -> fct (Or_error.ok_exn result)
let (>>|!) a fct = a >>| fun result -> fct (Or_error.ok_exn result)
;;

module Flags =
struct

  let ocamlopt_opt =
    Command.Spec.(flag "-cc"
            ~doc:"</path/to/ocamlopt.opt> set the ocaml native compiler"
            (required file))
  ;;

  let camlp4o_opt =
    Command.Spec.(flag "-pp"
            ~doc:"</path/to/camlp4o.opt> set the camlp4o native pre-processor"
            (optional file))
  ;;

  let ocamldep_opt =
    Command.Spec.(flag "-ocamldep"
            ~doc:"</path/to/ocamldep.opt> set the ocamldep native dependency generator"
            (optional file))
  ;;

  let pa_files =
    Command.Spec.(flag "-pa-cmxs"
            ~doc:"<file.cmxs> path to a native syntax extension plugin file"
            (listed file))
  ;;

  let target =
    Command.Spec.(flag "-o"
            ~doc:"<file.c> set the name of the c file to be created"
            (required file))
  ;;

  let wrap_symbol =
    Command.Spec.(flag "-wrap-symbol"
            ~doc:" generate __wrap_ocaml_plugin_archive instead of ocaml_plugin_archive"
            no_arg)
  ;;

  let verbose =
    Command.Spec.(flag "-verbose"
            ~doc:" be more verbose"
            no_arg)
  ;;

  let files =
    Command.Spec.(anon (sequence ("<embedded-file>" %: file)))
  ;;

  let all =
    Command.Spec.(
      empty
      +> ocamlopt_opt
      +> camlp4o_opt
      +> ocamldep_opt
      +> pa_files
      +> target
      +> wrap_symbol
      +> verbose
      +> files
    )
  ;;
end

let readme () = "\
This tool archives ocamlopt, cmis, camlp4 and preprocessors in a c file containing a big
static string. The resulting .o file should be linked with any executable that uses
Ocaml_plugin.Ocaml_compiler module. Or you can link your executable with a .o file
containing a dummy definition of the function ocaml_plugin_archive if you know you will
not need it."
;;

let summary =
  "tool to embed ocamlopt and cmi files into a c file"
;;

exception Suspicious_files_in_tar of string list * string list with sexp
exception Missing_files_in_tar of string list * string list with sexp
;;

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
;;

let escape =
  let a = Array.init ~f:(Printf.sprintf "\\x%02x") 256 in
  fun c -> a.(Char.to_int c)
;;

let generate_c_file wrap_symbol target tar =
  let module Digest = Plugin_cache.Digest in
  Monitor.try_with ~here:_here_ (fun () ->
    Reader.file_contents tar >>= fun str ->
    let str_digest = Digest.to_string (Digest.string str) in
    Writer.open_file target >>= fun w ->
    Writer.write w (String.concat ~sep:"\n" [
      "#include <string.h>";
      "#include <caml/mlvalues.h>";
      "#include <caml/memory.h>";
      "#include <caml/alloc.h>";
      "";
    ]);
    let write_static ~varname ~contents:str =
      Writer.write w (sprintf "static char %s[] = \"" varname);
      for i = 0 to String.length str - 1 do
        let c = str.[i] in
        Writer.write w (escape c)
      done;
      Writer.write w "\";\n";
    in
    let wrap = if wrap_symbol then "__wrap_" else "" in
    let define fctname ~varname ~contents:str =
      Writer.write w (String.concat ~sep:"\n" [
        "CAMLprim value " ^ wrap ^ fctname ^ " (value unit __attribute__ ((unused)))";
        "{";
        sprintf "  value v = caml_alloc_string(%d);" (String.length str);
        sprintf "  memcpy(String_val(v), %s, %d);" varname (String.length str);
        "  return(v);";
        "}";
        ""; (* error from gcc on centos5 if there is no newline at the end of file *)
      ]);
    in
    write_static  ~varname:"s"        ~contents:str;
    write_static  ~varname:"s_digest" ~contents:str_digest;
    define "ocaml_plugin_archive"        ~varname:"s"        ~contents:str;
    define "ocaml_plugin_archive_digest" ~varname:"s_digest" ~contents:str_digest;
    Writer.close w
  )
;;

let main ocamlopt_opt camlp4o_opt ocamldep_opt pa_files target wrap_symbol verbose files () =
  let _set_defaults_scope =
    Ocaml_plugin.Shell.set_defaults ~verbose ~echo:verbose ();
  in
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
  (match ocamldep_opt with
   | Some ocamldep_opt ->
     cp ~filename:ocamldep_opt ~basename:Ocaml_dynloader.ocamldep_opt >>| fun () ->
     Ocaml_dynloader.ocamldep_opt :: files
   | None ->
     return files
  ) >>= fun files ->
  (
    match camlp4o_opt with
    | None ->
      begin match pa_files with
      | [] -> Deferred.return []
      | _ -> failwith "syntax extension files given, but the -camlp4 flag is absent"
      end
    | Some camlp4o_opt ->
      Deferred.List.map ~how:`Parallel pa_files ~f:copy_file
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
  generate_c_file wrap_symbol target tar
  >>= function
  | Error exn ->
    Ocaml_plugin.Shell.rm ~r:() ~f:() [ tmpdir ]
    >>|! fun () ->
    raise exn
  | Ok () ->
    Ocaml_plugin.Tar.list tar >>=! fun check_files ->
    tar_check check_files files;
    Ocaml_plugin.Shell.rm ~r:() ~f:() [ tmpdir ] >>|! fun () -> ()
;;

let command =
  Command.async_basic ~summary ~readme Flags.all main
;;

let () =
  Exn.handle_uncaught ~exit:true (fun () -> Command.run command)
;;

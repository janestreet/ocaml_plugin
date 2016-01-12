open Core.Std
open Async.Std
open Ocaml_plugin.Std

let (>>=!) a fct = a >>= fun result -> fct (Or_error.ok_exn result)
let (>>|!) a fct = a >>| fun result -> fct (Or_error.ok_exn result)
;;

let readme () = "\
This tool archives ocamlopt, cmis, camlp4 and preprocessors in a c file containing a big
static string. The resulting .o file should be linked with any executable that uses
Ocaml_plugin.Ocaml_compiler module. Or you can link your executable with a .o file
containing a dummy definition of the function ocaml_plugin_archive if you know you will
not need it."
;;

exception Suspicious_files_in_tar of string list * string list [@@deriving sexp]
exception Missing_files_in_tar of string list * string list [@@deriving sexp]
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

let transfer_escaped ~file ~writer =
  Reader.with_file file ~f:(fun reader ->
    let transfered = ref 0 in
    Reader.read_one_chunk_at_a_time reader
      ~handle_chunk:(fun buf ~pos ~len ->
        for i = 0 to len - 1 do
          let c = buf.{pos + i} in
          if (!transfered + i) mod 20 = 0 then Writer.write writer "\"\n  \"";
          Writer.write writer (escape c)
        done;
        transfered := !transfered + len;
        Deferred.return `Continue)
    >>| function
    | `Eof -> !transfered
    | `Stopped _ | `Eof_with_unconsumed_data _ -> assert false
  )
;;

let ocaml_plugin_archive_template : (_, _, _, _) format4 = "\
CAMLprim value %socaml_plugin_archive (value unit __attribute__ ((unused)))
{
  intnat dim = %d;
  int flags = CAML_BA_UINT8 | CAML_BA_C_LAYOUT | CAML_BA_EXTERNAL;
  return caml_ba_alloc(flags, 1, s, &dim);
}
"

let ocaml_plugin_archive_digest_template : (_, _, _, _) format4 = "\
CAMLprim value %socaml_plugin_archive_digest (value unit __attribute__ ((unused)))
{
  return caml_copy_string(%S);
}
"

let generate_c_file wrap_symbol target tar =
  let module Digest = Plugin_cache.Digest in
  Monitor.try_with ~here:[%here] (fun () ->
    Digest.file tar >>=! fun file_digest ->
    let file_digest = Digest.to_string file_digest in
    Writer.with_file target ~f:(fun writer ->
      Writer.write writer (String.concat ~sep:"\n" [
         (* avoid making emacs die trying to highlight the huge string *)
        "/* -*- mode: fundamental; -*- */";
        "#include <string.h>";
        "#include <caml/mlvalues.h>";
        "#include <caml/memory.h>";
        "#include <caml/alloc.h>";
        "#include <caml/bigarray.h>";
        "";
      ]);
      Writer.write writer "static char s[] = \"";
      transfer_escaped ~file:tar ~writer >>| fun file_length ->
      Writer.write writer "\";\n\n";
      let wrap = if wrap_symbol then "__wrap_" else "" in
      Printf.ksprintf (Writer.write writer)
        ocaml_plugin_archive_template wrap file_length;
      Writer.write writer "\n";
      Printf.ksprintf (Writer.write writer)
        ocaml_plugin_archive_digest_template wrap file_digest;
    )
  )
;;

let command =
  Command.async
    ~summary:"tool to embed ocamlopt and cmi files into a c file"
    ~readme
    Command.Spec.(
      empty

      ++ step (fun k ocamlopt_opt -> k ~ocamlopt_opt)
      +> flag "-cc" (required file)
           ~doc:"</path/to/ocamlopt.opt> set the ocaml native compiler"

      ++ step (fun k camlp4o_opt -> k ~camlp4o_opt)
      +> flag "-pp" (optional file)
           ~doc:"</path/to/camlp4o.opt> set the camlp4o native pre-processor"

      ++ step (fun k ppx_exe -> k ~ppx_exe)
      +> flag "-ppx" (optional file)
           ~doc:"</path/to/ppx.exe> set the executable for ppx preprocessing"

      ++ step (fun k ocamldep_opt -> k ~ocamldep_opt)
      +> flag "-ocamldep" (optional file)
           ~doc:"</path/to/ocamldep.opt> set the ocamldep native dependency generator"

      ++ step (fun k pa_files -> k ~pa_files)
      +> flag "-pa-cmxs" (listed file)
           ~doc:"<file.cmxs> path to a native syntax extension plugin file"

      ++ step (fun k target -> k ~target)
      +> flag "-o" (required file)
           ~doc:"<file.c> set the name of the c file to be created"

      ++ step (fun k wrap_symbol -> k ~wrap_symbol)
      +> flag "-wrap-symbol" no_arg
           ~doc:" generate __wrap_ocaml_plugin_archive instead of ocaml_plugin_archive"

      ++ step (fun k verbose -> k ~verbose)
      +> flag "-verbose" no_arg ~doc:" be more verbose"

      ++ step (fun k extra_files -> k ~extra_files)
      +> anon (sequence ("<embedded-file>" %: file))
    )
  (fun
    ~ocamlopt_opt
    ~camlp4o_opt
    ~ppx_exe
    ~ocamldep_opt
    ~pa_files
    ~target
    ~wrap_symbol
    ~verbose
    ~extra_files
    () ->
  let _set_defaults_scope =
    Ocaml_plugin.Shell.set_defaults ~verbose ~echo:verbose ();
  in
  Ocaml_plugin.Shell.temp_dir ~in_dir:Filename.temp_dir_name () >>=! fun tmpdir ->
  let embedded_files = String.Hash_set.create () in
  let embed_file basename =
    if Hash_set.mem embedded_files basename
    then failwiths "cannot embed multiple files with the same basename"
           basename [%sexp_of: string];
    Hash_set.add embedded_files basename
  in
  let cp ~filename ~basename =
    embed_file basename;
    Ocaml_plugin.Shell.cp ~source:filename ~dest:(tmpdir ^/ basename)
    >>| ok_exn
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
  Deferred.List.map ~how:`Parallel extra_files ~f:copy_file >>= fun (_ : string list) ->
  cp ~filename:ocamlopt_opt ~basename:Ocaml_compiler.ocamlopt_opt >>= fun () ->
  begin match ocamldep_opt with
  | Some ocamldep_opt -> cp ~filename:ocamldep_opt ~basename:Ocaml_compiler.ocamldep_opt
  | None -> return ()
  end >>= fun () ->
  begin match ppx_exe with
  | Some ppx_exe -> cp ~filename:ppx_exe ~basename:Ocaml_compiler.ppx_exe
  | None -> return ()
  end >>= fun () ->
  begin match camlp4o_opt with
  | None ->
    begin match pa_files with
    | [] -> return ()
    | _ :: _ -> failwith "camlp4 extension files given (with -pa-cmxs), but -pp is absent"
    end
  | Some camlp4o_opt -> cp ~filename:camlp4o_opt ~basename:Ocaml_compiler.camlp4o_opt
  end >>= fun () ->
  Deferred.List.map ~how:`Parallel pa_files ~f:copy_file
  >>= fun pa_files ->
  let camlp4_is_embedded = Option.map camlp4o_opt ~f:(fun _ -> `pa_files pa_files) in
  let ppx_is_embedded = Option.is_some ppx_exe in
  let config = { Ocaml_compiler.Config. camlp4_is_embedded; ppx_is_embedded } in
  let config_file = tmpdir ^/ Ocaml_compiler.config_file in
  Writer.save_sexp ~hum:true config_file ([%sexp_of: Ocaml_compiler.Config.t] config)
  >>= fun () ->
  embed_file Ocaml_compiler.config_file;
  let tar = "a.tgz" in
  let embedded_files = Hash_set.to_list embedded_files in
  Ocaml_plugin.Tar.create ~working_dir:tmpdir ~files:embedded_files tar >>=! fun () ->
  let tar = tmpdir ^/ tar in
  generate_c_file wrap_symbol target tar
  >>= function
  | Error exn ->
    Ocaml_plugin.Shell.rm ~r:() ~f:() [ tmpdir ]
    >>|! fun () ->
    raise exn
  | Ok () ->
    Ocaml_plugin.Tar.list tar >>=! fun check_files ->
    tar_check check_files embedded_files;
    Ocaml_plugin.Shell.rm ~r:() ~f:() [ tmpdir ] >>|! fun () -> ()
  )
;;

let () =
  Exn.handle_uncaught ~exit:true (fun () -> Command.run command)
;;

open Core
open Async
open Ocaml_plugin.Std

let (>>=!) a fct = a >>= fun result -> fct (Or_error.ok_exn result)
let (>>|!) a fct = a >>| fun result -> fct (Or_error.ok_exn result)
;;

let readme () = "\
This tool archives ocamlopt, cmis and preprocessors in a c file containing a big static
 string. The resulting .o file should be linked with any executable that uses
 Ocaml_plugin.Ocaml_compiler module. Or you can link your executable with a .o file
 containing a dummy definition of the function ocaml_plugin_archive if you know you will
 not need it."
;;

let check_files_in_tar ~files_in_tar ~expected =
  let files_in_tar = List.sort ~cmp:String.compare files_in_tar in
  let files_in_tar_set = String.Set.of_list files_in_tar in
  if not (Set.equal files_in_tar_set expected)
  || List.length files_in_tar <> Set.length files_in_tar_set
  then
    raise_s
      [%sexp "Error checking files in tar"
           , { expected      : String.Set.t
             ; files_in_tar  : string list
             ; unknown_files = (Set.diff files_in_tar_set expected : String.Set.t)
             ; missing_files = (Set.diff expected files_in_tar_set : String.Set.t)
             }
           , [%here]
      ]
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
CAMLprim value ocaml_plugin_archive (value unit __attribute__ ((unused)))
{
  intnat dim = %d;
  int flags = CAML_BA_UINT8 | CAML_BA_C_LAYOUT | CAML_BA_EXTERNAL;
  return caml_ba_alloc(flags, 1, s, &dim);
}
"

let ocaml_plugin_archive_metadata_template : (_, _, _, _) format4 = "\
CAMLprim value ocaml_plugin_archive_metadata (value unit __attribute__ ((unused)))
{
  return caml_copy_string(%S);
}
"

let generate_c_file target ~tar ~metadata =
  Monitor.try_with ~here:[%here] (fun () ->
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
      Printf.ksprintf (Writer.write writer)
        ocaml_plugin_archive_template file_length;
      Writer.write writer "\n";
      Printf.ksprintf (Writer.write writer)
        ocaml_plugin_archive_metadata_template
        (Sexp.to_string_mach (Ocaml_compiler.Archive_metadata.sexp_of_t metadata));
    )
  )
;;

let command =
  Command.async ~readme ~summary:"tool to embed ocamlopt and cmi files into a c file"
    (let open Command.Let_syntax in
     let%map_open ocamlopt_opt =
       flag "-cc" (required file)
         ~doc:"</path/to/ocamlopt.opt> set the ocaml native compiler"
     and ppx_exe =
       flag "-ppx" (optional file)
         ~doc:"</path/to/ppx.exe> set the executable for ppx preprocessing"
     and ocamldep_opt =
       flag "-ocamldep" (optional file)
         ~doc:"</path/to/ocamldep.opt> set the ocamldep native dependency generator"
     and target =
       flag "-o" (required file)
         ~doc:"<file.c> set the name of the c file to be created"
     and verbose = flag "-verbose" no_arg ~doc:" be more verbose"
     and extra_files = anon (sequence ("<embedded-file>" %: file))
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let _set_defaults_scope =
         Ocaml_plugin.Shell.set_defaults ~verbose ~echo:verbose ();
       in
       Ocaml_plugin.Shell.temp_dir ~in_dir:Filename.temp_dir_name () >>=! fun tmpdir ->
       let embedded_files = String.Table.create () in
       let embed_file ~filename ~basename =
         match Hashtbl.add embedded_files ~key:basename ~data:filename with
         | `Ok -> ()
         | `Duplicate ->
           failwiths "cannot embed multiple files with the same basename"
             basename [%sexp_of: string]
       in
       let cp ~filename ~basename =
         embed_file ~filename ~basename;
         Ocaml_plugin.Shell.cp ~source:filename ~dest:(tmpdir ^/ basename)
         >>| ok_exn
       in
       let copy_file filename =
         let basename = Filename.basename filename in
         cp ~filename ~basename >>| fun () ->
         basename
       in
       let how = `Max_concurrent_jobs 10 in
       Deferred.List.map ~how extra_files ~f:copy_file >>= fun (_ : string list) ->
       cp ~filename:ocamlopt_opt ~basename:Ocaml_compiler.ocamlopt_opt >>= fun () ->
       begin match ocamldep_opt with
       | Some ocamldep_opt ->
         cp ~filename:ocamldep_opt ~basename:Ocaml_compiler.ocamldep_opt
       | None -> return ()
       end >>= fun () ->
       begin match ppx_exe with
       | Some ppx_exe -> cp ~filename:ppx_exe ~basename:Ocaml_compiler.ppx_exe
       | None -> return ()
       end >>= fun () ->
       let tar = "a.tgz" in
       Ocaml_plugin.Tar.create
         ~working_dir:tmpdir ~files:(Hashtbl.keys embedded_files) tar
       >>=! fun () ->
       Deferred.List.map ~how (Hashtbl.to_alist embedded_files)
         ~f:(fun (basename, filename) ->
           Plugin_cache.Digest.file filename >>|? fun digest -> basename, digest)
       >>| Or_error.combine_errors
       >>=! fun digests_by_basename ->
       let tar = tmpdir ^/ tar in
       generate_c_file target ~tar
         ~metadata:
           { ppx_is_embedded = Option.is_some ppx_exe
           ; archive_digests = String.Map.of_alist_exn digests_by_basename
           }
       >>= function
       | Error exn ->
         Ocaml_plugin.Shell.rm ~r:() ~f:() [ tmpdir ]
         >>|! fun () ->
         raise exn
       | Ok () ->
         Ocaml_plugin.Tar.list tar >>=! fun files_in_tar ->
         check_files_in_tar ~files_in_tar
           ~expected:(String.Set.of_hashtbl_keys embedded_files);
         Ocaml_plugin.Shell.rm ~r:() ~f:() [ tmpdir ] >>|! fun () -> ())
;;

let () = Command.run command

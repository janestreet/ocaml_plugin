open! Core
open! Async

let readme () =
  {|This tool archives ocamlopt, cmis and preprocessors in a c file containing a big static
 string. The resulting .o file should be linked with any executable that uses
 Ocaml_plugin.Compiler module. Or you can link your executable with a .o file
 containing a dummy definition of the function ocaml_plugin_archive if you know you will
 not need it.|}
;;

let check_files_in_tar ~files_in_tar ~expected =
  let files_in_tar = List.sort ~compare:String.compare files_in_tar in
  let files_in_tar_set = String.Set.of_list files_in_tar in
  if (not (Set.equal files_in_tar_set expected))
  || List.length files_in_tar <> Set.length files_in_tar_set
  then
    raise_s
      [%sexp
        "Error checking files in tar"
      , { expected : String.Set.t
        ; files_in_tar : string list
        ; unknown_files = (Set.diff files_in_tar_set expected : String.Set.t)
        ; missing_files = (Set.diff expected files_in_tar_set : String.Set.t)
        }
      , [%here]]
;;

let escape =
  let a = Array.init ~f:(Printf.sprintf "\\x%02x") 256 in
  fun c -> a.(Char.to_int c)
;;

let transfer_escaped ~file ~writer =
  Reader.with_file file ~f:(fun reader ->
    let transfered = ref 0 in
    match%map
      Reader.read_one_chunk_at_a_time reader ~handle_chunk:(fun buf ~pos ~len ->
        for i = 0 to len - 1 do
          let c = buf.{pos + i} in
          if (!transfered + i) mod 20 = 0 then Writer.write writer "\"\n  \"";
          Writer.write writer (escape c)
        done;
        transfered := !transfered + len;
        Deferred.return `Continue)
    with
    | `Eof -> !transfered
    | `Stopped _ | `Eof_with_unconsumed_data _ -> assert false)
;;

let ocaml_plugin_archive_template : (_, _, _, _) format4 =
  {|CAMLprim value ocaml_plugin_archive (value unit __attribute__ ((unused)))
{
  intnat dim = %d;
  int flags = CAML_BA_UINT8 | CAML_BA_C_LAYOUT | CAML_BA_EXTERNAL;
  return caml_ba_alloc(flags, 1, s, &dim);
}
|}
;;

let ocaml_plugin_archive_metadata_template : (_, _, _, _) format4 =
  {|CAMLprim value ocaml_plugin_archive_metadata (value unit __attribute__ ((unused)))
{
  return caml_copy_string(%S);
}
|}
;;

let generate_c_file target ~tar ~metadata =
  Monitor.try_with (fun () ->
    Writer.with_file target ~f:(fun writer ->
      Writer.write
        writer
        (String.concat
           ~sep:"\n"
           [ (* avoid making emacs die trying to highlight the huge string *)
             "/* -*- mode: fundamental; -*- */"
           ; "#include <string.h>"
           ; "#include <caml/mlvalues.h>"
           ; "#include <caml/memory.h>"
           ; "#include <caml/alloc.h>"
           ; "#include <caml/bigarray.h>"
           ; ""
           ]);
      Writer.write writer "static char s[] = \"";
      let%map file_length = transfer_escaped ~file:tar ~writer in
      Writer.write writer "\";\n\n";
      Printf.ksprintf (Writer.write writer) ocaml_plugin_archive_template file_length;
      Writer.write writer "\n";
      Printf.ksprintf
        (Writer.write writer)
        ocaml_plugin_archive_metadata_template
        (Sexp.to_string_mach (Ocaml_plugin.Compiler.Archive_metadata.sexp_of_t metadata))))
;;

let command =
  Command.async
    ~readme
    ~summary:"tool to embed ocamlopt and cmi files into a c file"
    (let%map_open.Command ocamlopt_opt =
       flag
         "-cc"
         (required Filename_unix.arg_type)
         ~doc:"</path/to/ocamlopt.opt> set the ocaml native compiler"
     and ppx_exe =
       flag
         "-ppx"
         (optional Filename_unix.arg_type)
         ~doc:"</path/to/ppx.exe> set the executable for ppx preprocessing"
     and ocamldep_opt =
       flag
         "-ocamldep"
         (optional Filename_unix.arg_type)
         ~doc:"</path/to/ocamldep.opt> set the ocamldep native dependency generator"
     and target =
       flag
         "-o"
         (required Filename_unix.arg_type)
         ~doc:"<file.c> set the name of the c file to be created"
     and verbose = flag "-verbose" no_arg ~doc:" be more verbose"
     and extra_files = anon (sequence ("<embedded-file>" %: Filename_unix.arg_type)) in
     fun () ->
       let _set_defaults_scope =
         Ocaml_plugin.Private.Shell.set_defaults ~verbose ~echo:verbose ()
       in
       let%bind tmpdir =
         Ocaml_plugin.Private.Shell.temp_dir ~in_dir:Filename.temp_dir_name () >>| ok_exn
       in
       let embedded_files = String.Table.create () in
       let embed_file ~filename ~basename =
         match Hashtbl.add embedded_files ~key:basename ~data:filename with
         | `Ok -> ()
         | `Duplicate ->
           raise_s
             [%sexp
               "cannot embed multiple files with the same basename", (basename : string)]
       in
       let cp ~filename ~basename =
         embed_file ~filename ~basename;
         Ocaml_plugin.Private.Shell.cp ~source:filename ~dest:(tmpdir ^/ basename)
         >>| ok_exn
       in
       let copy_file filename =
         let basename = Filename.basename filename in
         let%map () = cp ~filename ~basename in
         basename
       in
       let how = `Max_concurrent_jobs 10 in
       let%bind (_ : string list) = Deferred.List.map ~how extra_files ~f:copy_file in
       let%bind () =
         cp ~filename:ocamlopt_opt ~basename:Ocaml_plugin.Compiler.ocamlopt_opt
       in
       let%bind () =
         match ocamldep_opt with
         | Some ocamldep_opt ->
           cp ~filename:ocamldep_opt ~basename:Ocaml_plugin.Compiler.ocamldep_opt
         | None -> return ()
       in
       let%bind () =
         match ppx_exe with
         | Some ppx_exe -> cp ~filename:ppx_exe ~basename:Ocaml_plugin.Compiler.ppx_exe
         | None -> return ()
       in
       let tar = "a.tgz" in
       let%bind () =
         Ocaml_plugin.Private.Tar.create
           ~working_dir:tmpdir
           ~files:(Hashtbl.keys embedded_files)
           tar
         >>| ok_exn
       in
       let%bind digests_by_basename =
         Deferred.List.map
           ~how
           (Hashtbl.to_alist embedded_files)
           ~f:(fun (basename, filename) ->
             Ocaml_plugin.Plugin_cache.Digest.file filename
             >>|? fun digest -> basename, digest)
         >>| Or_error.combine_errors
         >>| ok_exn
       in
       let tar = tmpdir ^/ tar in
       match%bind
         generate_c_file
           target
           ~tar
           ~metadata:
             { ppx_is_embedded = Option.is_some ppx_exe
             ; archive_digests = String.Map.of_alist_exn digests_by_basename
             }
       with
       | Error exn ->
         let%map () = Ocaml_plugin.Private.Shell.rm ~r:() ~f:() [ tmpdir ] >>| ok_exn in
         raise exn
       | Ok () ->
         let%bind files_in_tar = Ocaml_plugin.Private.Tar.list tar >>| ok_exn in
         check_files_in_tar
           ~files_in_tar
           ~expected:(String.Set.of_hashtbl_keys embedded_files);
         Ocaml_plugin.Private.Shell.rm ~r:() ~f:() [ tmpdir ] >>| ok_exn)
    ~behave_nicely_in_pipeline:false
;;


let () = Command_unix.run command

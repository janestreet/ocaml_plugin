open! Core
open! Async
open! Import

let tar_id = "dynlink.tgz"
;;

let ocamlopt_opt = "ocamlopt.opt"
let ocamldep_opt = "ocamldep.opt"
let ppx_exe      = "ppx.exe"
;;

let persistent_archive_subdir = "compiler"
;;

module Archive_metadata = struct
  (* Not trying to be stable here: it's simpler and it's not clear why stability would be
     useful: if this type changes, there is no hope that Info.is_up_to_date could return
     [true] (because at least ocaml_plugin.cmi changes since the type is exposed, causing
     archive_digests to differ), so whether or not we can read old versions of the type,
     we'll re-extract the archive. *)
  type t =
    { ppx_is_embedded    : bool
    ; archive_digests    : Plugin_cache.Digest.t String.Map.t
    }
  [@@deriving sexp]
end

(* Map of the directories contents:
   - build_dir: /tmp/ocaml_plugin_XXXXX/{m_dyn_1_.ml,m_dyn_1_.o,m_dyn_1_.cmx,m_dyn_1_.cmxs,...}
     Deleted on clean up.

   - compiler_dir:
     Either /tmp/ocaml_plugin_XXXXX/{ocamlopt.opt,pervasives.cmi,pa_sexp_conv.cmo,...}
     or $user_specified_dir/compiler/{archive-info.sexp,the other files}.
     Locked if it is shared using $user_specified_dir/compiler.lock. Deleted on clean up
     if they are the same directory otherwise deleted when the digest of the archive
     doesn't match the info anymore.

   - cache dir:
     $user_specified_dir/cmxs-cache/{cache-info.sexp, abcd-efgh-ijkl-mnop.cmxs}
     copied there from build_dir
     Locked because it can be shared using $user_specified_dir/cmxs-cache.lock.

   Both locks are released on clean up. *)

module Archive_lock = struct
  type t =
    | Cleaned
    | Cleaning of unit Deferred.Or_error.t
    | Locked of string
end

type t =
  { loader       : Dynloader.t
  ; archive_lock : Archive_lock.t ref
  }
[@@deriving fields]

let clean t =
  Dynloader.clean t.loader >>= fun r1 ->
  (match t.archive_lock.contents with
   | Archive_lock.Cleaned -> Deferred.Or_error.ok_unit
   | Archive_lock.Cleaning def -> def
   | Archive_lock.Locked lock_filename ->
     let clean = Lock_file_async.Nfs.unlock lock_filename in
     t.archive_lock.contents <- Archive_lock.Cleaning clean;
     clean >>| fun res ->
     t.archive_lock.contents <- Archive_lock.Cleaned;
     res)
  >>| fun r2 ->
  Or_error.combine_errors_unit
    [ r1
    ; r2
    ]
;;

(* This external declaration should be implemented by the c files generated by
   ocaml_embed_compiler.exe, which is normally called when using an "embed" entry
   in the jbuild. There is a default implementation in ocaml_fake_archive.c too. *)
external archive : unit -> Bigstring.t = "ocaml_plugin_archive"
let archive () =
  let bstr = archive () in
  let dummy = "dummy" in
  if Bigstring.length bstr = String.length dummy &&
     String.equal (Bigstring.to_string bstr) dummy then
    None
  else
    Some bstr
;;

external archive_metadata_binding : unit -> string = "ocaml_plugin_archive_metadata"
let archive_metadata =
  lazy (
    let str = archive_metadata_binding () in
    let dummy = "dummy" in
    if String.equal str dummy
    then Or_error.error_string "\
This executable does not have an embedded archive, although this is required when using \
[Ocaml_plugin].  A likely cause is that the build of the binary is missing the step \
involving [ocaml_embed_compiler]."
    else Ok (Sexp.of_string_conv_exn str [%of_sexp: Archive_metadata.t]))
;;

let embedded_files () =
  Or_error.map (force archive_metadata) ~f:(fun m -> m.archive_digests)
;;

let () =
  match Core.Sys.getenv "OCAML_PLUGIN_DUMP_ARCHIVE" with
  | None -> ()
  | Some _ ->
    (* This is a way of extracting the archive from the executable. It can be used like
       this: OCAML_PLUGIN_DUMP_ARCHIVE= ./run.exe | tar -xz
       We exit to avoid running any side effects that could be done later at toplevel. *)
    (match force archive_metadata with
     | Error _ -> Core.Printf.eprintf "No archive metadata\n%!"
     | Ok archive_metadata ->
       Core.Printf.eprintf !"archive metadata: %{sexp:Archive_metadata.t}\n%!"
         archive_metadata);
    (match archive () with
     | None -> Core.Printf.printf "No archive\n%!"
     | Some bstr -> Bigstring.really_output stdout bstr; Out_channel.flush stdout);
    Core.Caml.exit 0
;;

let save_archive_to destination =
  Deferred.Or_error.try_with (fun () ->
    match archive () with
    | None -> failwith "There is no embedded compiler in the current executable"
    | Some contents -> Writer.with_file_atomic destination ~f:(fun w ->
      Writer.schedule_bigstring w contents;
      Deferred.unit))
;;

type 'a create_arguments = (
  ?persistent_archive_dirpath:string
  -> 'a
) Dynloader.create_arguments

module Plugin_archive : sig
  val extract :
    archive_lock:Archive_lock.t ref
    -> persistent:bool
    -> string
    -> unit Deferred.Or_error.t
end = struct
  module Info = struct
    type t =
      { infos          : (string * Sexp.t) list
      ; build_info     : Sexp.t
      ; archive_metadata : Archive_metadata.t
      }
    [@@deriving sexp]

    let t_of_sexp = Sexp.of_sexp_allow_extra_fields_recursively t_of_sexp

    let info_file_name = "archive-info.sexp"
    let info_file dir = dir ^/ info_file_name
    ;;

    let create () =
      return (force archive_metadata) >>=? fun archive_metadata ->
      Deferred.Or_error.try_with ~extract_exn:true (fun () ->
        Unix.getlogin () >>| fun login ->
        [ "version"  , sexp_of_string Params.version
        ; "login"    , sexp_of_string login
        ; "hostname" , sexp_of_string (Unix.gethostname ())
        ; "sys_argv" , [%sexp_of: string array] Sys.argv
        ]
      ) >>|? fun infos ->
      let build_info = Params.build_info_as_sexp in
      { infos
      ; build_info
      ; archive_metadata
      }
    ;;

    let info_file_perm = 0o644
    ;;

    let save dir =
      create () >>=? fun t ->
      Deferred.Or_error.try_with ~extract_exn:true (fun () ->
        Writer.save_sexp ~perm:info_file_perm (info_file dir) (sexp_of_t t)
      )
    ;;

    let load dir =
      Deferred.Or_error.try_with ~extract_exn:true (fun () ->
        Reader.load_sexp_exn (info_file dir) t_of_sexp
      )
    ;;

    let is_up_to_date t ~dir =
      match force archive_metadata with
      | Error _ as error -> return error
      | Ok archive_metadata ->
        let digests = archive_metadata.archive_digests in
        if [%compare.equal: Plugin_cache.Digest.t String.Map.t]
             digests t.archive_metadata.archive_digests
        then (
          (* Here we assume people won't change the contents of our files, but we could
             not make such an assumption and check the digests instead. Or make the files
             read-only.  We expect neither missing files (obviously), neither additional
             files (like extra cmis because they can impact the build). *)
          Deferred.Or_error.try_with (fun () -> Sys.readdir dir)
          >>|? fun files ->
          let files_extracted =
            Set.diff
              (String.Set.of_list (Array.to_list files))
              (String.Set.of_list [ info_file_name; tar_id ])
          in
          let files_we_would_extract = Map.key_set digests in
          String.Set.equal files_extracted files_we_would_extract)
        else return (Ok false)
    ;;
  end

  let extract_throttle = Throttle.Sequencer.create ~continue_on_error:true ()
  ;;

  let extract ~archive_lock ~persistent compiler_dir =
    let extract () =
      if_ persistent (fun () ->
        let lock_filename = compiler_dir ^ ".lock" in
        Monitor.try_with_or_error (fun () ->
          Unix.mkdir ~p:() ~perm:0o755 (Filename.dirname lock_filename)) >>=? fun () ->
        Lock_file_async.Nfs.create lock_filename >>=? fun () ->
        archive_lock := Archive_lock.Locked lock_filename;
        Shell.rm ~r:() ~f:() [ compiler_dir ] >>=? fun () ->
        Monitor.try_with_or_error (fun () ->
          Unix.mkdir ~p:() ~perm:0o755 compiler_dir)
      ) >>=? fun () ->
      let destination = compiler_dir ^/ tar_id in
      save_archive_to destination >>=? fun () ->
      Tar.extract ~working_dir:compiler_dir destination >>=? fun () ->
      if_ persistent (fun () -> Info.save compiler_dir)
    in
    if persistent
    then Throttle.enqueue extract_throttle (fun () ->
      Info.load compiler_dir >>= function
      | Error _ -> extract ()
      | Ok info ->
        Info.is_up_to_date info ~dir:compiler_dir
        >>= function
        | Ok true -> Deferred.Or_error.ok_unit
        | Error _ | Ok false -> extract ()
    )
    else
      extract ()
  ;;
end

let create
      ?in_dir
      ?in_dir_perm
      ?include_directories
      ?custom_warnings_spec
      ?strict_sequence
      ?cmx_flags
      ?cmxs_flags
      ?trigger_unused_value_warnings_despite_mli
      ?use_cache
      ?persistent_archive_dirpath
      () =
  let archive_lock = ref Archive_lock.Cleaned in
  (match persistent_archive_dirpath with
   | None     -> Deferred.return (Ok None)
   | Some path ->
     Shell.absolute_pathname path >>|? fun path ->
     Some (path ^/ persistent_archive_subdir)
  ) >>=? fun persistent_archive_dirpath ->
  let in_compiler_dir exec =
    Option.value persistent_archive_dirpath ~default:"." ^/ exec
  in
  let include_directories =
    match persistent_archive_dirpath with
    | None -> include_directories
    | Some dir -> Some (dir :: Option.value include_directories ~default:[])
  in
  let ocamlopt_opt = in_compiler_dir ocamlopt_opt in
  let ocamldep_opt = in_compiler_dir ocamldep_opt in
  let nostdlib flags = "-nostdlib" :: Option.value ~default:[] flags in
  let cmx_flags = nostdlib cmx_flags in
  let cmxs_flags = nostdlib cmxs_flags in
  let preprocessor =
    match force archive_metadata with
    | Error _ as error -> error
    | Ok { ppx_is_embedded; archive_digests = _ } ->
      if ppx_is_embedded
      then Ok (Dynloader.Preprocessor.Ppx { ppx_exe = in_compiler_dir ppx_exe })
      else Ok Dynloader.Preprocessor.No_preprocessing
  in
  return preprocessor
  >>=? fun preprocessor ->
  let compilation_config = { Dynloader.Compilation_config.preprocessor } in
  let initialize ~directory:build_dir =
    let persistent, compiler_dir =
      match persistent_archive_dirpath with
      | None                 -> false, build_dir
      | Some archive_dirpath -> true,  archive_dirpath
    in
    Plugin_archive.extract ~archive_lock ~persistent compiler_dir
  in
  Dynloader.create
    ?in_dir
    ?in_dir_perm
    ?include_directories
    ?custom_warnings_spec
    ?strict_sequence
    ~cmx_flags
    ~cmxs_flags
    ?trigger_unused_value_warnings_despite_mli
    ?use_cache
    ~initialize
    ~compilation_config
    ~ocamlopt_opt
    ~ocamldep_opt
    ()
  >>=? fun loader ->
  let compiler =
    { loader
    ; archive_lock
    }
  in
  Deferred.return (Ok (`this_needs_manual_cleaning_after compiler))
;;

let created_but_not_cleaned = Bag.create ()
;;

let () =
  (* I think we can rely on the at_shutdown handlers only firing in the current process
     and not in the forks.  In that case, worse things could happen than deleting the
     compiler under our feet. *)
  Shutdown.at_shutdown (fun () ->
    Deferred.List.iter (Bag.to_list created_but_not_cleaned) ~f:(fun compiler ->
      clean compiler >>| function
      | Ok ()   -> ()
      | Error _ -> ()))
;;

let is_shutting_down () =
  match Shutdown.shutting_down () with
  | `No    -> false
  | `Yes _ -> true
;;

let with_compiler
      ?in_dir
      ?in_dir_perm
      ?include_directories
      ?custom_warnings_spec
      ?strict_sequence
      ?cmx_flags
      ?cmxs_flags
      ?trigger_unused_value_warnings_despite_mli
      ?use_cache
      ?persistent_archive_dirpath
      ~f
      ()
  =
  if is_shutting_down ()
  then return (Or_error.error_s [%sexp "Shutting_down", [%here]])
  else (
    create
      ?in_dir
      ?in_dir_perm
      ?include_directories
      ?custom_warnings_spec
      ?strict_sequence
      ?cmx_flags
      ?cmxs_flags
      ?trigger_unused_value_warnings_despite_mli
      ?use_cache
      ?persistent_archive_dirpath
      ()
    >>=? fun (`this_needs_manual_cleaning_after compiler) ->
    if is_shutting_down ()
    then (
      clean compiler >>=? fun () ->
      return (Or_error.error_s [%sexp "Shutting_down", [%here]]))
    else (
      let bag_elem = Bag.add created_but_not_cleaned compiler in
      Deferred.Or_error.try_with_join ~extract_exn:true (fun () -> f compiler)
      >>= fun result ->
      Bag.remove created_but_not_cleaned bag_elem;
      clean compiler >>| fun r2 ->
      match result, r2 with
      | Ok result, Ok ()          -> Ok result
      | Ok _, (Error _ as error)  -> error
      | Error e1, Error e2        -> Error (Error.of_list [e1; e2])
      | Error _ as error, Ok ()   -> error))
;;

let make_load_ocaml_src_files load_ocaml_src_files =
  let aux
        ?in_dir
        ?in_dir_perm
        ?include_directories
        ?custom_warnings_spec
        ?strict_sequence
        ?cmx_flags
        ?cmxs_flags
        ?trigger_unused_value_warnings_despite_mli
        ?use_cache
        ?persistent_archive_dirpath
        files =
    let f compiler =
      let loader = loader compiler in
      load_ocaml_src_files loader files
    in
    with_compiler
      ?in_dir
      ?in_dir_perm
      ?include_directories
      ?custom_warnings_spec
      ?strict_sequence
      ?cmx_flags
      ?cmxs_flags
      ?trigger_unused_value_warnings_despite_mli
      ?use_cache
      ?persistent_archive_dirpath
      ~f
      ()
  in
  aux
;;

let make_check_plugin_cmd
      ~check_ocaml_src_files
      ~load_ocaml_src_files () =
  let execute_plugin_toplevel_switch = "-execute-plugin-toplevel" in
  Command.async ~summary:"Check a plugin for compilation errors"
    ~readme:(fun () -> String.concat [ "\
This command checks that a plugin compiles.  It either succeeds quietly, or outputs
compilation errors and fails.

When it is deemed safe to execute the toplevel of a plugin, one can supply the switch
[" ; execute_plugin_toplevel_switch ; "] to check for runtime exceptions at toplevel." ])
    (let open Command.Let_syntax in
     let%map_open plugin_filenames = anon (sequence ("path/to/plugin.ml" %: Filename.arg_type))
     and execute_plugin_toplevel =
       flag execute_plugin_toplevel_switch no_arg
         ~doc:" Run the plugin's toplevel to check for runtime errors"
     and use_ocamldep =
       flag "-ocamldep" no_arg
         ~doc:" Use ocamldep. Expect only the main file in the remaining arguments"
     and is_verbose =
       flag "-verbose" no_arg
         ~doc:" Be more verbose"
     in
     fun () ->
       let open! Deferred.Let_syntax in
       let f compiler =
         let loader = loader compiler in
         (if use_ocamldep
          then
            (match plugin_filenames with
             | [ main ] -> Dynloader.find_dependencies loader main
             | [] | _ :: _ :: _ ->
               return
                 (Or_error.error "Give only the main file when using option -ocamldep"
                    plugin_filenames [%sexp_of: string list]))
          else return (Ok plugin_filenames)
         ) >>=? fun plugin_filenames ->
         if is_verbose then
           Print.printf "checking: %s\n%!" (String.concat ~sep:" " plugin_filenames);
         if execute_plugin_toplevel
         then
           load_ocaml_src_files loader plugin_filenames
           >>| Or_error.map ~f:ignore
         else
           check_ocaml_src_files loader plugin_filenames
       in
       with_compiler ~f ()
       >>| function
       | Ok () ->
         if is_verbose then Print.printf "ok\n%!";
         Shutdown.shutdown 0
       | Error err ->
         Print.eprintf "%s\n%!" (Error.to_string_hum err);
         Shutdown.shutdown 1)
;;

module type S = sig
  type t

  val load_ocaml_src_files : (
    string list -> t Deferred.Or_error.t
  ) create_arguments

  val load_ocaml_src_files_without_running_them : (
    string list -> (unit -> t) Deferred.Or_error.t
  ) create_arguments

  val check_ocaml_src_files : (
    string list -> unit Deferred.Or_error.t
  ) create_arguments

  val check_plugin_cmd : unit -> Command.t

  module Load : Dynloader.S with type t := t
end

module Make (X:Dynloader.Module_type) = struct
  module Load = Dynloader.Make(X)
  let load_ocaml_src_files  = make_load_ocaml_src_files Load.load_ocaml_src_files
  let load_ocaml_src_files_without_running_them =
    make_load_ocaml_src_files Load.load_ocaml_src_files_without_running_them
  let check_ocaml_src_files = make_load_ocaml_src_files Load.check_ocaml_src_files
  ;;

  let check_plugin_cmd =
    make_check_plugin_cmd
      ~check_ocaml_src_files:Load.check_ocaml_src_files
      ~load_ocaml_src_files:Load.load_ocaml_src_files
  ;;
end

module Side_effect = struct
  module Load = Dynloader.Side_effect
  let load_ocaml_src_files = make_load_ocaml_src_files Load.load_ocaml_src_files
  let load_ocaml_src_files_without_running_them =
    make_load_ocaml_src_files Load.load_ocaml_src_files_without_running_them
  let check_ocaml_src_files = make_load_ocaml_src_files Load.check_ocaml_src_files
  ;;

  let check_plugin_cmd =
    make_check_plugin_cmd
      ~check_ocaml_src_files:Load.check_ocaml_src_files
      ~load_ocaml_src_files:Load.load_ocaml_src_files
  ;;
end

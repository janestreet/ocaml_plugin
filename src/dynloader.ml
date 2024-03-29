open! Core
open! Async
module E = Deferred.Or_error

let default_disabled_warnings = [ 4; 29; 40; 41; 42; 44; 45; 48; 58; 66; 69; 70 ]

(* The default policy about warnings *)
let warnings_spec ~disabled_warnings =
  let ignores =
    List.map disabled_warnings ~f:(fun i -> "-" ^ Int.to_string i) |> String.concat
  in
  "+a" ^ ignores
;;

let default_warnings_spec = warnings_spec ~disabled_warnings:default_disabled_warnings
let index = ref 0

module Ppx = struct
  type t = { ppx_exe : string }
end

module Preprocessor = struct
  type t =
    | No_preprocessing
    | Ppx of Ppx.t
end

module Compilation_config = struct
  type t = { preprocessor : Preprocessor.t }

  let default = { preprocessor = No_preprocessing }
end

(* Default values to use for those binaries if their path is not specified. *)
module Default_binaries = struct
  let ocamlopt_opt = "ocamlopt.opt"
  let ocamldep_opt = "ocamldep.opt"
end

module Compilation_directory : sig
  type t = private { directory : string }

  val create
    :  initialize:(directory:string -> unit Or_error.t Deferred.t)
    -> in_dir:string
    -> in_dir_perm:Unix.file_perm option
    -> t Or_error.t Deferred.t
end = struct
  type t = { directory : string }

  let info_file_name = "info"
  let info_file dir = dir ^/ info_file_name

  module Info : sig
    val save : info_file:string -> unit Or_error.t Deferred.t
  end = struct
    (*
       save some debug info in the builddir in case this doesn't get cleaned
    *)

    type t =
      { login : string
      ; hostname : string
      ; pid : Pid.t
      ; sys_argv : string array
      ; version : string
      ; build_info : Sexp.t
      }
    [@@deriving sexp_of]

    let create () =
      Deferred.Or_error.try_with ~rest:`Log ~extract_exn:true (fun () ->
        let hostname = Unix.gethostname () in
        let pid = Unix.getpid () in
        let build_info = Params.build_info_as_sexp in
        let version = Params.version in
        let sys_argv = Sys.get_argv () in
        let%map login = Unix.getlogin () in
        let t = { login; hostname; pid; build_info; version; sys_argv } in
        sexp_of_t t)
    ;;

    let get = Lazy_deferred.create create

    let save ~info_file =
      let%bind.E info = Lazy_deferred.force_exn get in
      Deferred.Or_error.try_with ~rest:`Log ~extract_exn:true (fun () ->
        Writer.save_sexp ~hum:true info_file info)
    ;;
  end

  let create ~initialize ~in_dir ~in_dir_perm =
    let%bind.E directory = Shell.temp_dir ~in_dir ?perm:in_dir_perm () in
    match%bind
      let%bind.E () = initialize ~directory in
      let%bind.E () = Info.save ~info_file:(info_file directory) in
      return (Ok { directory })
    with
    | Ok _ as ok -> return ok
    | Error e ->
      (* if something failed, the rest of the code of ocaml dynloader will never know
         about the directory so we should delete it since no one else can. *)
      (match%map Shell.rm ~r:() ~f:() [ directory ] with
       | Ok () -> Error e
       | Error e2 -> Error (Error.of_list [ e; e2 ]))
  ;;
end

(* every t has a different compilation directory *)
type t =
  { mutable cleaned : bool
  ; cmx_flags : string list
  ; cmxs_flags : string list
  ; trigger_unused_value_warnings_despite_mli : bool
  ; compilation_directory : Compilation_directory.t Or_error.t Lazy_deferred.t
  ; compilation_config : Compilation_config.t
  ; include_directories : string list
  ; ocamlopt_opt : string
  ; ocamldep_opt : string
  ; cache : Plugin_cache.t Or_error.t Lazy_deferred.t option
  }
[@@deriving fields]

type dynloader = t

let next_filename () =
  let s = Printf.sprintf "m_dyn_%d_.ml" !index in
  incr index;
  s
;;

type 'a create_arguments =
  ?in_dir:string
  -> ?in_dir_perm:Unix.file_perm
  -> ?include_directories:string list
  -> ?custom_warnings_spec:string
  -> ?strict_sequence:bool
  -> ?cmx_flags:string list
  -> ?cmxs_flags:string list
  -> ?trigger_unused_value_warnings_despite_mli:bool
  -> ?use_cache:Plugin_cache.Config.t
  -> 'a

let create
      ?(in_dir = Filename.temp_dir_name)
      ?in_dir_perm
      ?(include_directories = [])
      ?(custom_warnings_spec = default_warnings_spec)
      ?(strict_sequence = true)
      ?(cmx_flags = [])
      ?(cmxs_flags = [])
      ?(trigger_unused_value_warnings_despite_mli = false)
      ?use_cache
      ?(initialize = fun ~directory:_ -> return (Ok ()))
      ?(compilation_config = Compilation_config.default)
      ?(ocamlopt_opt = Default_binaries.ocamlopt_opt)
      ?(ocamldep_opt = Default_binaries.ocamldep_opt)
      ()
  =
  let cmx_flags =
    [ "-no-alias-deps"; "-w"; custom_warnings_spec; "-warn-error"; "+a" ] @ cmx_flags
  in
  let cmx_flags =
    if strict_sequence then "-strict-sequence" :: cmx_flags else cmx_flags
  in
  if not Dynlink.is_native
  then return (Or_error.error_s [%sexp "Ocaml_plugin only works in native code"])
  else (
    let%bind.E include_directories = Shell.absolute_pathnames include_directories in
    let cache =
      Option.map use_cache ~f:(fun cache_config ->
        Lazy_deferred.create (fun () -> Plugin_cache.create cache_config))
    in
    let compilation_directory =
      Lazy_deferred.create (fun () ->
        Compilation_directory.create ~initialize ~in_dir ~in_dir_perm)
    in
    let cleaned = false in
    let t =
      { cleaned
      ; cmx_flags
      ; cmxs_flags
      ; trigger_unused_value_warnings_despite_mli
      ; compilation_directory
      ; compilation_config
      ; include_directories
      ; ocamlopt_opt
      ; ocamldep_opt
      ; cache
      }
    in
    Deferred.return (Ok t))
;;

let clean_compilation_directory t =
  if not (Lazy_deferred.is_forced t.compilation_directory)
  then return (Ok ())
  else (
    match%bind Lazy_deferred.force_exn t.compilation_directory with
    | Error _ ->
      (* if t.compilation_config failed, then either we couldn't create the temporary
         directory, in which case there is nothing to clean, or we failed after in
         which case the temporary directory was cleaned there. Either way, things are
         fine. *)
      return (Ok ())
    | Ok { directory; _ } -> Shell.rm ~r:() ~f:() [ directory ])
;;

let clean_plugin_cache t =
  match t.cache with
  | None -> return (Ok ())
  | Some cache ->
    if not (Lazy_deferred.is_forced cache)
    then return (Ok ())
    else (
      match%bind Lazy_deferred.force_exn cache with
      | Error _ -> return (Ok ())
      | Ok cache -> Plugin_cache.clean cache)
;;

let clean t =
  if t.cleaned
  then return (Ok ())
  else (
    t.cleaned <- true;
    let%bind r1 = clean_compilation_directory t in
    let%map r2 = clean_plugin_cache t in
    Or_error.combine_errors_unit [ r1; r2 ])
;;

module Univ_constr = struct
  type 'a t = 'a Type_equal.Id.t

  let name = "Ocaml_plugin.Dynloader.Univ_constr.t"
  let create () = Type_equal.Id.create ~name sexp_of_opaque
end

module type Module_type = sig
  type t

  val t_repr : string
  val univ_constr : t Univ_constr.t
  val univ_constr_repr : string
end

type packed_plugin = E : 'a Univ_constr.t * (unit -> 'a) -> packed_plugin

exception Return_plugin of packed_plugin

let return_plugin (type a) (constr : a Univ_constr.t) (fct : unit -> a) =
  raise (Return_plugin (E (constr, fct)))
;;

let preprocess_shebang ~first_line =
  if String.is_prefix first_line ~prefix:"#!"
  then sprintf "(* %S *)" first_line
  else first_line
;;

let include_directories dirs = List.concat_map dirs ~f:(fun dir -> [ "-I"; dir ])

let make_pp_args ?(map_exe = Fn.id) preprocessor =
  let call prog args =
    String.concat ~sep:" " (List.map ~f:Filename.quote (map_exe prog :: args))
  in
  match (preprocessor : Preprocessor.t) with
  | No_preprocessing -> []
  | Ppx { ppx_exe } -> [ "-pp"; call ppx_exe [ "-dump-ast" ] ]
;;

module Compile : sig
  val copy_files
    :  trigger_unused_value_warnings_despite_mli:bool
    -> compilation_directory:Compilation_directory.t
    -> Plugin_uuid.t
    -> string Deferred.Or_error.t

  val blocking_dynlink : string -> packed_plugin Or_error.t
  val dynlink : string -> packed_plugin Deferred.Or_error.t

  val compile_and_load_file
    :  t
    -> compilation_directory:Compilation_directory.t
    -> basename:Core.String.Hash_set.elt
    -> (string * packed_plugin) Async.Deferred.Or_error.t
end = struct
  let output_line out_channel line =
    Out_channel.output_string out_channel line;
    Out_channel.output_char out_channel '\n'
  ;;

  let output_in_channel out_channel in_channel =
    (* This is to support scripts that have a shebang line *)
    match In_channel.input_line in_channel with
    | None -> ()
    | Some first_line ->
      output_line out_channel (preprocess_shebang ~first_line);
      In_channel.iter_lines in_channel ~f:(output_line out_channel)
  ;;

  let permission = 0o600

  (* Normally adding a signature on an implementation adds warnings, but
     here because no signature means that it defaults to [sig end], adding a
     signature removes warnings. *)

  let ocaml_plugin_gen_sig_prefix = "OCAML_PLUGIN__sig_"

  let copy_files
        ~trigger_unused_value_warnings_despite_mli
        ~(compilation_directory : Compilation_directory.t)
        plugin_uuid
    =
    let fct () =
      let repr = Plugin_uuid.repr plugin_uuid in
      let with_bundle out_channel bundle =
        let `ml filename, `mli intf_filename_opt, `module_name module_name =
          Ml_bundle.to_pathnames bundle
        in
        let output_struct ~sig_name_opt =
          (match sig_name_opt with
           | None -> Core.Printf.fprintf out_channel "module %s = struct\n" module_name
           | Some sig_name ->
             Core.Printf.fprintf
               out_channel
               "module %s : %s = struct\n"
               module_name
               sig_name);
          Core.Printf.fprintf out_channel "#1 %S\n" filename;
          In_channel.with_file filename ~binary:false ~f:(output_in_channel out_channel);
          Core.Printf.fprintf out_channel "\nend\n"
        in
        match intf_filename_opt with
        | None -> output_struct ~sig_name_opt:None
        | Some intf_filename ->
          let sig_name = ocaml_plugin_gen_sig_prefix ^ module_name in
          Core.Printf.fprintf out_channel "module type %s = sig\n" sig_name;
          Core.Printf.fprintf out_channel "#1 %S\n" intf_filename;
          In_channel.with_file
            ~binary:false
            intf_filename
            ~f:(output_in_channel out_channel);
          Core.Printf.fprintf out_channel "\nend\n";
          output_struct ~sig_name_opt:(Some sig_name);
          if not trigger_unused_value_warnings_despite_mli
          then
            Core.Printf.fprintf
              out_channel
              "let _avoid_warnings = (module %s : %s)\n"
              module_name
              sig_name
      in
      let target = next_filename () in
      let full_target = compilation_directory.directory ^/ target in
      let with_out_channel out_channel =
        let bundles = Plugin_uuid.ml_bundles plugin_uuid in
        let last_bundle =
          match List.last bundles with
          | None -> raise_s [%sexp "Ocaml_plugin: No_file_to_compile"]
          | Some last -> last
        in
        let main_module_name = Ml_bundle.module_name last_bundle in
        Core.Printf.fprintf
          out_channel
          ("module F () : sig\n" ^^ "  module %s : %s\n" ^^ "end\n = struct\n")
          main_module_name
          (Plugin_uuid.Repr.t repr);
        List.iter bundles ~f:(with_bundle out_channel);
        Core.Printf.fprintf out_channel "end\n";
        Core.Printf.fprintf
          out_channel
          ("let () =\n"
           ^^ "  let module R = Ocaml_plugin.Dynloader in\n"
           ^^ "  R.return_plugin %s (fun () ->\n"
           ^^ "    let module M = F() in\n"
           ^^ "    (module M.%s : %s))\n")
          (Plugin_uuid.Repr.univ_constr repr)
          main_module_name
          (Plugin_uuid.Repr.t repr)
      in
      Out_channel.with_file full_target ~binary:false ~perm:permission ~f:with_out_channel;
      target
    in
    Deferred.Or_error.try_with ~rest:`Log ~extract_exn:true (fun () -> In_thread.run fct)
  ;;


  (* Dynlink has the following not really wanted property: dynlinking a file with a given
     filename only works properly the first time. Further dynlinks with the same filename
     (even a different file) will not load the new module but instead execute the initial
     module. Since ocaml_plugin need to be able to load cmxs coming from ml files with the
     same name (several variations of config.ml for instance), what we do is give unique
     name to each cmxs that we produce: files in the cache have their uuid in the name,
     and files not in the cache are called $tmp_dir/something_$fresh.cmxs.
     We can't have several Ocaml_plugin.Dynloader.t use the same directory, because
     ocaml_plugin always create a fresh directory in which to put its files.

     The other related problem is that if we dynlink the same filename multiple times,
     even if that filename has the same contents every time, we risk memory corruption.
     The problem is that dlopen the second time returns the same pointers as the first
     time (even with loadfile_private, which does a dlopen(..., RTLD_LOCAL)
     underneath. [man dlopen] says "If the same library is loaded again with dlopen(), the
     same file handle is returned"), which means that the second load initializes the
     previously loaded module instead of a new one. This can lead to the gc roots
     corruption, which leads to memory corruption. This situation can happen when loading
     modules from the plugin-cache, where loads of the same plugin use the same path.  To
     avoid this, we keep track of every module we've already loaded (which is a memory
     leak, but it's just an amplification of the one in dlopen, not a new one), so we can
     only load them once per process (while still repeating the side effects in the users'
     source files). *)
  let blocking_dynlink_exn =
    let plugins_loaded_privately : (string, packed_plugin) Hashtbl.t =
      Hashtbl.create (module String)
    in
    fun file ->
      match Hashtbl.find plugins_loaded_privately file with
      | Some packed_plugin -> packed_plugin
      | None ->
        (match Dynlink.loadfile_private file with
         | () -> raise_s [%sexp "Ocaml_plugin: Plugin_did_not_return"]
         | exception
             ( Dynlink.Error
                 (Library's_module_initializers_failed (Return_plugin packed_plugin))
             | Return_plugin packed_plugin ) ->
           Hashtbl.set plugins_loaded_privately ~key:file ~data:packed_plugin;
           packed_plugin
         | exception Dynlink.Error e ->
           raise_s
             [%sexp "Ocaml_plugin: Dynlink_error", (Dynlink.error_message e : string)])
  ;;

  let dynlink =
    let dynlink_sequencer = Sequencer.create ~continue_on_error:true () in
    fun cmxs_filename ->
      (* Ensure we can't dynlink multiple modules concurrently, as Dynlink doesn't support
         such things, as in it leads to segfaults. *)
      Throttle.enqueue dynlink_sequencer (fun () ->
        Deferred.Or_error.try_with ~rest:`Log ~extract_exn:true (fun () ->
          In_thread.run (fun () -> blocking_dynlink_exn cmxs_filename)))
  ;;

  let blocking_dynlink cmxs_filename =
    Or_error.try_with (fun () -> blocking_dynlink_exn cmxs_filename)
  ;;

  let compile_and_load_file
        t
        ~compilation_directory:{ Compilation_directory.directory = working_dir }
        ~basename
    =
    let basename_without_ext =
      try Filename.chop_extension basename with
      | Invalid_argument _ -> basename
    in
    let ext = Printf.sprintf "%s.%s" basename_without_ext in
    let ml = ext "ml" in
    let cmx = ext "cmx" in
    let cmxs = ext "cmxs" in
    let pp_args = make_pp_args t.compilation_config.preprocessor in
    let create_cmx_args =
      pp_args
      @ include_directories t.include_directories
      @ t.cmx_flags
      @ [ "-c"; "-o"; cmx; ml ]
    in
    let create_cmxs_args = t.cmxs_flags @ [ "-shared"; cmx; "-o"; cmxs ] in
    let%bind.E () =
      Shell.run ~quiet_or_error:true ~working_dir t.ocamlopt_opt create_cmx_args
    in
    let%bind.E () =
      Shell.run ~quiet_or_error:true ~working_dir t.ocamlopt_opt create_cmxs_args
    in
    let cmxs = working_dir ^/ cmxs in
    let%map.E packed_plugin = dynlink cmxs in
    cmxs, packed_plugin
  ;;
end

let copy_source_files_to_working_dir ~source_dir ~working_dir =
  Deferred.Or_error.try_with ~rest:`Log (fun () ->
    let%bind all_ocaml_files =
      Sys.ls_dir source_dir
      >>| List.filter ~f:(fun file ->
        (* We filter out some files created by emacs with names like ".#fool.ml" that we
           would fail to read because they are dead symlinks. *)
        (not (String.is_prefix file ~prefix:"."))
        && (String.is_suffix file ~suffix:".ml"
            || String.is_suffix file ~suffix:".mli"))
    in
    Deferred.List.iter ~how:`Sequential all_ocaml_files ~f:(fun file ->
      let source_file_name = source_dir ^/ file in
      Reader.with_file source_file_name ~f:(fun source_file ->
        Writer.with_file (working_dir ^/ file) ~f:(fun dest_file ->
          Writer.writef dest_file "#1 %S\n" source_file_name;
          match%bind Reader.read_line source_file with
          | `Eof -> Deferred.unit
          | `Ok first_line ->
            Writer.write_line dest_file (preprocess_shebang ~first_line);
            Pipe.iter_without_pushback
              (Reader.lines source_file)
              ~f:(Writer.write_line dest_file)))))
;;

let find_dependencies t filename =
  if t.cleaned
  then return (Or_error.error_s [%sexp "Usage_of_cleaned_dynloader", [%here]])
  else (
    let%bind.E () =
      if Filename.check_suffix filename ".ml"
      then return (Ok ())
      else
        return
          (Or_error.errorf
             "Ocaml_plugin.Dynloader.find_dependencies: argument %S is not an ml file"
             filename)
    in
    let%bind.E { directory = base_dir } =
      Lazy_deferred.force_exn t.compilation_directory
    in
    let%bind.E filename = Shell.absolute_pathname filename in
    let source_dir = Filename.dirname filename in
    let target = Filename.chop_extension (Filename.basename filename) in
    let in_base_dir file =
      (* our [working_dir] is not [base_dir], and [file] is relative to [base_dir] if it
         is not an absolute path and not an invocation to something in $PATH *)
      if (not (Filename.is_absolute file)) && String.mem file '/'
      then base_dir ^/ file
      else file
    in
    let pp_args = make_pp_args ~map_exe:in_base_dir t.compilation_config.preprocessor in
    (* we create a new directory under [base_dir] as ocamldep's working directory, when
       we copy files, we strip the shebang line. *)
    let%bind.E working_dir =
      Shell.temp_dir ~in_dir:base_dir ~prefix:"ocamldep" ~suffix:"" ()
    in
    let%bind.E () = copy_source_files_to_working_dir ~source_dir ~working_dir in
    let%bind.E compilation_units =
      Ocamldep.find_dependencies
        ~prog:(in_base_dir t.ocamldep_opt)
        ~args:pp_args
        ~working_dir
        ~target
    in
    (* convert the topological order of compilation units into a list of files *)
    Deferred.List.map ~how:`Sequential compilation_units ~f:(fun compilation_unit ->
      (* return files from [source_dir] *)
      let ml = source_dir ^/ compilation_unit ^ ".ml" in
      let mli = ml ^ "i" in
      match%map Sys.file_exists mli with
      | `Yes -> Ok [ mli; ml ]
      | `No -> Ok [ ml ]
      | `Unknown -> Or_error.errorf "File in unknown state: %s" mli)
    >>| Or_error.all
    >>|? List.concat)
;;

let load_ocaml_src_files_plugin_uuid ~repr t filenames =
  if t.cleaned
  then return (Or_error.error_s [%sexp "Usage_of_cleaned_dynloader", [%here]])
  else (
    let compile_without_cache ml_bundles =
      let%bind.E compilation_directory =
        Lazy_deferred.force_exn t.compilation_directory
      in
      let plugin_uuid = Plugin_uuid.create ~repr ~ml_bundles () in
      let trigger_unused_value_warnings_despite_mli =
        t.trigger_unused_value_warnings_despite_mli
      in
      let%bind.E basename =
        Compile.copy_files
          ~compilation_directory
          ~trigger_unused_value_warnings_despite_mli
          plugin_uuid
      in
      let%map.E res = Compile.compile_and_load_file t ~compilation_directory ~basename in
      plugin_uuid, res
    in
    let%bind.E filenames = Shell.absolute_pathnames filenames in
    let%bind.E ml_bundles = Ml_bundle.from_filenames filenames in
    match t.cache with
    | None ->
      let%map.E _, (cmxs_filename, packed_plugin) = compile_without_cache ml_bundles in
      `cmxs_filename cmxs_filename, packed_plugin
    | Some cache ->
      let%bind.E cache = Lazy_deferred.force_exn cache in
      let%bind.E sources = Plugin_cache.digest ml_bundles in
      let refresh_cache () =
        let%bind.E plugin_uuid, (cmxs_filename, packed_plugin) =
          compile_without_cache ml_bundles
        in
        let%map.E () = Plugin_cache.add cache sources plugin_uuid cmxs_filename in
        `cmxs_filename cmxs_filename, packed_plugin
      in
      (match Plugin_cache.find cache sources with
       | Some plugin ->
         let cmxs_filename = Plugin_cache.Plugin.cmxs_filename plugin in
         (match%bind Compile.dynlink cmxs_filename with
          | Ok packed_plugin ->
            Deferred.Or_error.return (`cmxs_filename cmxs_filename, packed_plugin)
          | Error _ as error ->
            if Plugin_cache.Plugin.was_compiled_by_current_exec plugin
            then
              (* Rebuilding the cmxs from scratch would lead to the exact same file since
                 we have the same exec that the one that was used to build the same
                 sources. Thus, the result of the dynlink would the same anyway, something
                 else should be wrong. *)
              Deferred.return error
            else
              (* In that case, since the exec has changed since the last time it was used
                 to build this cache, we might have a chance that dynlinking a freshly
                 rebuilt cmxs file would actually succeed.  In the case where the plugin
                 dynlinked normally but raises an exception at toplevel, we will go
                 through this branch and recompile it a second time. It is probably fine
                 though. *)
              refresh_cache ())
       | None -> refresh_cache ()))
;;

module type S = sig
  type t

  val load_ocaml_src_files : dynloader -> string list -> t Deferred.Or_error.t

  val load_ocaml_src_files_without_running_them
    :  dynloader
    -> string list
    -> (unit -> t) Deferred.Or_error.t

  val check_ocaml_src_files : dynloader -> string list -> unit Deferred.Or_error.t

  module Expert : sig
    val compile_ocaml_src_files_into_cmxs_file
      :  dynloader
      -> string list
      -> output_file:string
      -> unit Deferred.Or_error.t

    val load_cmxs_file : string -> t Or_error.t Deferred.t
    val blocking_load_cmxs_file : string -> t Or_error.t
  end
end

module Make (X : Module_type) = struct
  let type_check plugin_type =
    (* There is an hidden invariant there: if the OCaml compilation succeed, that means
       that the loaded module has the type represented in [X.repr], so the [Univ.match_]
       will succeed.  Of course this is only true is the user gave a valid [Module_type]
       in the first place. *)
    match Type_equal.Id.same_witness plugin_type X.univ_constr with
    | Some witness -> Ok witness
    | None ->
      Or_error.error_s
        [%sexp "Type_mismatch", (X.t_repr : string), (X.univ_constr_repr : string)]
  ;;

  let load_and_type_ocaml_src_files_without_running_them t filenames =
    let repr = Plugin_uuid.Repr.create ~t:X.t_repr ~univ_constr:X.univ_constr_repr in
    let%bind.E cmxs_filename, E (plugin_type, make_plugin) =
      load_ocaml_src_files_plugin_uuid ~repr t filenames
    in
    return
      (match type_check plugin_type with
       | Error _ as e -> e
       | Ok Type_equal.T -> Ok (cmxs_filename, (make_plugin : unit -> X.t)))
  ;;

  let load_ocaml_src_files_without_running_them t filenames =
    let%map.E `cmxs_filename _, make_plugin =
      load_and_type_ocaml_src_files_without_running_them t filenames
    in
    make_plugin
  ;;

  let run make_plugin =
    try Ok (make_plugin ()) with
    | exn ->
      Or_error.tag
        (Or_error.of_exn exn)
        ~tag:"Exception while executing the plugin's toplevel"
  ;;

  let load_ocaml_src_files t filenames =
    let%bind.E `cmxs_filename _, make_plugin =
      load_and_type_ocaml_src_files_without_running_them t filenames
    in
    Deferred.return (run make_plugin)
  ;;

  let check_ocaml_src_files t filenames =
    let%map.E (_ : unit -> X.t) = load_ocaml_src_files_without_running_them t filenames in
    ()
  ;;

  module Expert = struct
    let compile_ocaml_src_files_into_cmxs_file t filenames ~output_file =
      let%bind.E `cmxs_filename cmxs_filename, (_ : unit -> X.t) =
        load_and_type_ocaml_src_files_without_running_them t filenames
      in
      Shell.cp ~source:cmxs_filename ~dest:output_file
    ;;

    let blocking_load_cmxs_file cmxs_filename : X.t Or_error.t =
      if not (Scheduler.is_ready_to_initialize ())
      then
        Or_error.error_string
          "blocking_load_cmxs_file can only be called when Async scheduler isn't \
           initialized"
      else (
        match Compile.blocking_dynlink cmxs_filename with
        | Error _ as e -> e
        | Ok (E (plugin_type, make_plugin)) ->
          (match type_check plugin_type with
           | Error _ as e -> e
           | Ok Type_equal.T -> run make_plugin))
    ;;

    let load_cmxs_file cmxs_filename =
      let%bind.E (E (plugin_type, make_plugin)) = Compile.dynlink cmxs_filename in
      match type_check plugin_type with
      | Error _ as e -> return e
      | Ok Type_equal.T ->
        let make_plugin : unit -> X.t = make_plugin in
        Deferred.return (run make_plugin)
    ;;
  end
end

module type Side_effect = sig end

let side_effect_univ_constr = Univ_constr.create ()

module Side_effect_loader = Make (struct
    type t = (module Side_effect)

    let t_repr = "Ocaml_plugin.Dynloader.Side_effect"
    let univ_constr = side_effect_univ_constr
    let univ_constr_repr = "Ocaml_plugin.Dynloader.side_effect_univ_constr"
  end)

module Side_effect = struct
  open Side_effect_loader

  let check_ocaml_src_files = check_ocaml_src_files

  let load_ocaml_src_files t filenames =
    let%map.E (_ : (module Side_effect)) = load_ocaml_src_files t filenames in
    ()
  ;;

  let load_ocaml_src_files_without_running_them t filenames =
    let%map.E f = load_ocaml_src_files_without_running_them t filenames in
    ();
    fun () -> ignore (f () : (module Side_effect))
  ;;

  module Expert = struct
    let compile_ocaml_src_files_into_cmxs_file =
      Expert.compile_ocaml_src_files_into_cmxs_file
    ;;

    let blocking_load_cmxs_file filename =
      Expert.blocking_load_cmxs_file filename
      |> (Or_error.ignore_m : (module Side_effect) Or_error.t -> unit Or_error.t)
    ;;

    let load_cmxs_file filename =
      let%map.E (_ : (module Side_effect)) = Expert.load_cmxs_file filename in
      ()
    ;;
  end
end

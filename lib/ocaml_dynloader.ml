open Core.Std
open Async.Std

(* The default policy about warnings *)
let default_warnings_spec = "@a-4-29-40-41-42-44-45-48"
;;

let index = ref 0
;;

module Config = struct
  type t = {
    pa_files : string list ;
  } with sexp
end

(* every t has a different compilation directory *)
type t = {
  mutable cleaned : bool;
  cmx_flags : string list;
  cmxs_flags : string list;
  trigger_unused_value_warnings_despite_mli : bool;
  compilation_config : (string * Config.t option) Or_error.t Lazy_deferred.t;
  include_directories : string list;
  ocamlopt_opt : string;
  camlp4o_opt : string;
  ocamldep_opt : string;
  pa_files : string list; (* Usually contains explicit provided preprocessors. The ones
                             from the config will be added right before generating the
                             command lines *)
  cache : Plugin_cache.t Or_error.t Lazy_deferred.t option;
  run_plugin_toplevel: [ `In_async_thread | `Outside_of_async ];
} with fields

type dynloader = t

let next_filename () =
  let s = Printf.sprintf "m_dyn_%d_.ml" !index in
  incr index;
  s
;;

let ocamlopt_opt = "ocamlopt.opt"
let camlp4o_opt  = "camlp4o.opt"
let ocamldep_opt = "ocamldep.opt"
;;

module Compilation_config = struct

  let info_file_name = "info"
  ;;

  let info_file dir = dir ^/ info_file_name
  ;;

  module Info = struct
    (*
      save some debug info in the builddir in case this doesn't get cleaned
    *)

    type t = {
      login : string;
      hostname : string;
      pid : Pid.t;
      sys_argv : string array;
      version : string;
      build_info : string;
    } with sexp_of

    let create () =
      Deferred.Or_error.try_with ~extract_exn:true (fun () ->
        let hostname = Unix.gethostname () in
        let pid = Unix.getpid () in
        let build_info = Params.build_info in
        let version = Params.version in
        let sys_argv = Sys.argv in
        Unix.getlogin () >>| fun login ->
        let t = {
          login;
          hostname;
          pid;
          build_info;
          version;
          sys_argv;
        } in
        sexp_of_t t
      )
    ;;

    let get = Lazy_deferred.create create
    ;;

    let save info_file =
      Lazy_deferred.force_exn get >>=? fun info ->
      Deferred.Or_error.try_with ~extract_exn:true (fun () ->
        Writer.save_sexp ~hum:true info_file info
      )
    ;;
  end

  let lazy_create ~initialize_compilation_callback ~in_dir =
    let compute () =
      Shell.temp_dir ~in_dir >>=? fun directory ->
      begin
        (match initialize_compilation_callback with
        | None -> return (Ok None)
        | Some task -> task ~directory) >>=? fun config_opt ->
        let info_file = info_file directory in
        Info.save info_file >>=? fun () ->
        return (Ok (directory, config_opt))
      end >>= function
      | Ok _ as ok -> return ok
      | Error e ->
        (* if something failed, the rest of the code of ocaml dynloader will never know
           about the directory so we should delete it since no one else can. *)
        Shell.rm ~r:() ~f:() [directory] >>| function
        | Ok () -> Error e
        | Error e2 -> Error (Error.of_list [e; e2])
    in
    Lazy_deferred.create compute
  ;;
end

let update_with_config t = function
  | None -> t
  | Some conf ->
    let pa_files = conf.Config.pa_files @ t.pa_files in
    { t with
      pa_files;
    }
;;

exception Is_not_native with sexp

type 'a create_arguments =
  ?in_dir:string
  -> ?include_directories:string list
  -> ?custom_warnings_spec:string
  -> ?strict_sequence:bool
  -> ?cmx_flags:string list
  -> ?cmxs_flags:string list
  -> ?trigger_unused_value_warnings_despite_mli:bool
  -> ?use_cache:Plugin_cache.Config.t
  -> ?run_plugin_toplevel: [ `In_async_thread | `Outside_of_async ]
  -> 'a

let create
    ?(in_dir = Filename.temp_dir_name)
    ?(include_directories = [])
    ?(custom_warnings_spec = default_warnings_spec)
    ?(strict_sequence = true)
    ?(cmx_flags = [])
    ?(cmxs_flags = [])
    ?(trigger_unused_value_warnings_despite_mli = false)
    ?use_cache
    ?(run_plugin_toplevel = `In_async_thread)
    ?initialize_compilation_callback
    ?(ocamlopt_opt = ocamlopt_opt)
    ?(camlp4o_opt = camlp4o_opt)
    ?(ocamldep_opt = ocamldep_opt)
    ?(pa_files = [])
    ()
    =
  let cmx_flags =
    [ "-w" ; custom_warnings_spec ; "-warn-error" ; "+a" ]
    @ cmx_flags
  in
  let cmx_flags =
    if strict_sequence
    then "-strict-sequence" :: cmx_flags
    else cmx_flags
  in
  if not Dynlink.is_native then Deferred.Or_error.of_exn Is_not_native else
  Shell.absolute_pathnames include_directories >>=? fun include_directories ->
  let cache = Option.map use_cache ~f:(fun cache_config ->
    Lazy_deferred.create (fun () -> Plugin_cache.create cache_config))
  in
  let compilation_config =
    Compilation_config.lazy_create ~initialize_compilation_callback ~in_dir
  in
  let cleaned = false in
  let t =
    {
      cleaned;
      cmx_flags;
      cmxs_flags;
      trigger_unused_value_warnings_despite_mli;
      compilation_config;
      include_directories;
      ocamlopt_opt;
      camlp4o_opt;
      ocamldep_opt;
      pa_files;
      cache;
      run_plugin_toplevel;
    } in
  Deferred.return (Ok t)
;;

let clean_compilation_directory t =
  if not (Lazy_deferred.is_forced t.compilation_config) then return (Ok ())
  else begin
    Lazy_deferred.force_exn t.compilation_config >>= function
    | Error _ ->
      (* if t.compilation_config failed, then either we couldn't create the temporary
         directory, in which case there is nothing to clean, or we failed after in
         which case the temporary directory was cleaned there. Either way, things are
         fine. *)
      return (Ok ())
    | Ok (directory, _) ->
      Shell.rm ~r:() ~f:() [directory]
  end
;;

let clean_plugin_cache t =
  match t.cache with
  | None -> return (Ok ())
  | Some cache ->
    if not (Lazy_deferred.is_forced cache) then return (Ok ())
    else begin
      Lazy_deferred.force_exn cache >>= function
      | Error _ -> return (Ok ())
      | Ok cache ->
        Plugin_cache.clean cache
    end
;;

let clean t =
  if t.cleaned then return (Ok ())
  else begin
    t.cleaned <- true;
    clean_compilation_directory t >>= fun r1 ->
    clean_plugin_cache t >>| fun r2 ->
    Or_error.combine_errors_unit [
      r1;
      r2;
    ]
  end
;;

module Univ_constr = struct
  type 'a t = 'a Type_equal.Id.t
  let name = "Ocaml_plugin.Std.Ocaml_dynloader.Univ_constr.t"
  let create () = Type_equal.Id.create ~name sexp_of_opaque
end

module type Module_type =
sig
  type t
  val t_repr : string
  val univ_constr : t Univ_constr.t
  val univ_constr_repr : string
end

exception No_file_to_compile with sexp
exception Dynlink_error of string with sexp

exception Plugin_did_not_return with sexp
exception Return_plugin of (unit -> Univ.t)

;;

let return_plugin (type a) (constr : a Univ_constr.t) (fct : unit -> a) =
  let fct () = Univ.create constr (fct ()) in
  raise (Return_plugin fct)
;;

module Compile : sig

  val copy_files :
    trigger_unused_value_warnings_despite_mli:bool
    -> working_dir:string
    -> Plugin_uuid.t
    -> string Deferred.Or_error.t

  val dynlink :
    ?export:bool
    -> string -> (unit -> Univ.t) Deferred.Or_error.t

  val compile_and_load_file :
    working_dir:string
    -> t
    -> ?export:bool
    -> basename:Core.Std.String.Hash_set.elt
    -> (string * (unit -> Univ.t)) Async.Std.Deferred.Or_error.t

end = struct

  let output_in_channel out_channel in_channel =
    let content = In_channel.input_all in_channel in
    Out_channel.output_string out_channel content
  ;;

  let permission = 0o600
  ;;

  let copy_files ~trigger_unused_value_warnings_despite_mli ~working_dir plugin_uuid =
    let fct () =
      let repr = Plugin_uuid.repr plugin_uuid in
      let with_bundle ~outer_sig ~last_module out_channel bundle =
        if not outer_sig || not trigger_unused_value_warnings_despite_mli || last_module
        then begin
          let `ml filename, `mli intf_filename_opt, `module_name module_name =
            Ml_bundle.to_pathnames bundle
          in
          Printf.fprintf out_channel "module %s" module_name;
          begin match intf_filename_opt with
          | None ->
            if last_module
            then
              Printf.fprintf out_channel " : %s" (Plugin_uuid.Repr.t repr)
            else
              (* Normally adding a signature on an implementation adds warnings, but
                 here because no signature means that it defaults to [sig end], adding a
                 signature removes warnings. *)
              if outer_sig then
                Printf.fprintf out_channel " : sig end"
          | Some intf_filename ->
            if outer_sig && trigger_unused_value_warnings_despite_mli && last_module
            then
              Printf.fprintf out_channel " : %s" (Plugin_uuid.Repr.t repr)
            else begin
              Printf.fprintf out_channel " : sig\n";
              Printf.fprintf out_channel "#1 %S\n" intf_filename;
              In_channel.with_file ~binary:false intf_filename
                ~f:(output_in_channel out_channel);
              Printf.fprintf out_channel "\nend";
            end
          end;
          if outer_sig
          then
            Printf.fprintf out_channel "\n"
          else begin
            Printf.fprintf out_channel " = struct\n";
            Printf.fprintf out_channel "#1 %S\n" filename;
            In_channel.with_file filename
              ~binary:false
              ~f:(output_in_channel out_channel);
            Printf.fprintf out_channel "\nend\n";
          end
        end
      in
      let target = next_filename () in
      let full_target = working_dir ^/ target in
      let with_out_channel out_channel =
        let utils, last =
          match List.rev (Plugin_uuid.ml_bundles plugin_uuid) with
          | [] -> raise No_file_to_compile
          | last :: utils -> List.rev utils, last
        in
        let main_module_name = Ml_bundle.module_name last in
        let iter_bundle ~outer_sig =
          List.iter utils ~f:(with_bundle ~outer_sig ~last_module:false out_channel);
          with_bundle ~outer_sig ~last_module:true out_channel last;
        in
        Printf.fprintf out_channel "module F (X : sig end) : sig\n";
        iter_bundle ~outer_sig:true;
        Printf.fprintf out_channel "end\n = struct\n";
        iter_bundle ~outer_sig:false;
        Printf.fprintf out_channel "end\n";
        Printf.fprintf out_channel (
          "let () =\n" ^^
          "  let module R = Ocaml_plugin.Ocaml_dynloader in\n" ^^
          "  R.return_plugin %s (fun () ->\n" ^^
          "    let module M = F(struct end) in\n" ^^
          "    (module M.%s : %s))\n"
        )
          (Plugin_uuid.Repr.univ_constr repr)
          main_module_name
          (Plugin_uuid.Repr.t repr)
      in
      Out_channel.with_file full_target
        ~binary:false
        ~perm:permission
        ~f:with_out_channel
      ;
      target
    in
    Deferred.Or_error.try_with ~extract_exn:true (fun () -> In_thread.run fct)
  ;;


  (* Dynlink has the following not really wanted property: dynlinking a file with a given
     filename only works properly the first time. Further dynlinks with the same filename
     (even a different file) will not load the new module but instead execute the initial
     module. Since ocaml_plugin need to be able to load cmxs coming from ml files with the
     same name (several variations of config.ml for instance), what we do is give unique
     name to each cmxs that we produce: files in the cache have their uuid in the name,
     and files not in the cache are called $tmp_dir/something_$fresh.cmxs.
     We can't have several Ocaml_dynloader.t use the same directory, because ocaml_plugin
     always create a fresh directory in which to put its files. *)
  let blocking_dynlink ?(export=false) file =
    let loadfile = if export then Dynlink.loadfile else Dynlink.loadfile_private in
    try
      loadfile file;
      raise Plugin_did_not_return
    with
    | Dynlink.Error e ->
      raise (Dynlink_error (Dynlink.error_message e))
    | Return_plugin univ -> univ
  ;;

  let dynlink ?export cmxs_filename =
    let fct () = blocking_dynlink ?export cmxs_filename in
    Deferred.Or_error.try_with ~extract_exn:true (fun () -> In_thread.run fct)
  ;;

  let compile_and_load_file ~working_dir t ?export ~basename =
    let basename_without_ext =
      try Filename.chop_extension basename
      with Invalid_argument _ -> basename
    in
    let ext  = Printf.sprintf "%s.%s" basename_without_ext in
    let ml   = ext "ml" in
    let cmx  = ext "cmx" in
    let cmxs = ext "cmxs" in
    let include_directories =
      let f dir = [ "-I" ; dir ] in
      List.concat_map ~f t.include_directories
    in
    let pp_args = match t.pa_files with
      | [] -> []
      | cmxs ->
        [ "-pp" ;
          String.concat (t.camlp4o_opt :: include_directories @ cmxs) ~sep:" "
        ]
    in
    let create_cmx_args =
      pp_args @ include_directories @ t.cmx_flags @ [
        "-c";
        "-o"; cmx;
        ml;
      ]
    in
    let create_cmxs_args =
      t.cmxs_flags @ [
        "-shared"; cmx;
        "-o"; cmxs;
      ]
    in
    Shell.run ~quiet_or_error:true ~working_dir t.ocamlopt_opt create_cmx_args
    >>=? fun () ->
    Shell.run ~quiet_or_error:true ~working_dir t.ocamlopt_opt create_cmxs_args
    >>=? fun () ->
    let cmxs = working_dir ^/ cmxs in
    dynlink ?export cmxs >>|? fun univ ->
    cmxs, univ
  ;;
end

exception Usage_of_cleaned_dynloader with sexp
;;

let find_dependencies t filename =
  if t.cleaned then Deferred.Or_error.of_exn Usage_of_cleaned_dynloader else
    (if Filename.check_suffix filename ".ml"
     then return (Ok ())
     else return (Or_error.errorf "Ocaml_dynloader.find_dependencies: \
                                   argument %S is not an ml file" filename)
    ) >>=? fun () ->
    Lazy_deferred.force_exn t.compilation_config >>=? fun (base_dir, config_opt) ->
    let t = update_with_config t config_opt in
    Shell.absolute_pathname filename >>=? fun filename ->
    let working_dir = Filename.dirname filename in
    let target = Filename.chop_extension (Filename.basename filename) in
    let in_base_dir file =
      (* our [working_dir] is supplied by user and is not [base_dir], and [file] is
         relative to [base_dir] if it is not an absolute path and not an invocation to
         something in $PATH *)
      if not (Filename.is_absolute file) && String.mem file '/'
      then base_dir ^/ file
      else file
    in
    let pp_args = match t.pa_files with
      | [] -> []
      | cmxs ->
        let include_directories =
          let f dir = [ "-I" ; dir ] in
          List.concat_map ~f (base_dir :: t.include_directories)
        in
        [ "-pp" ;
          String.concat (in_base_dir t.camlp4o_opt :: include_directories @ cmxs) ~sep:" "
        ]
    in
    (* The reason we don't copy these ml input files to base_dir is simply because we
       don't have to. There's no gain in copying them. ocamldep with camlp4o doesn't write
       any temp files in the working directory, so it will work also in read-only
       locations. This invariant is checked going forward with a unit test that loads ml
       files from a read-only folder. *)
    Ocamldep.find_dependencies
      ~prog:(in_base_dir t.ocamldep_opt)
      ~args:pp_args
      ~working_dir
      ~target
    >>=? fun compilation_units ->
    (* convert the topological order of compilation units into a list of files *)
    (Deferred.List.map compilation_units ~f:(fun compilation_unit ->
       let ml  = working_dir ^/ (compilation_unit ^ ".ml")  in
       let mli = ml ^ "i" in
       Sys.file_exists mli >>| function
       | `Yes ->  Ok [ mli; ml ]
       | `No -> Ok [ ml ]
       | `Unknown -> Or_error.errorf "File in unknown state: %s" mli
     ) >>| Or_error.all)
    >>|? List.concat
;;

let load_ocaml_src_files_plugin_uuid ~repr t filenames =
  if t.cleaned then Deferred.Or_error.of_exn Usage_of_cleaned_dynloader else
  let compile_without_cache ml_bundles =
    Lazy_deferred.force_exn t.compilation_config >>=? fun (working_dir, config_opt) ->
    let t = update_with_config t config_opt in
    let plugin_uuid = Plugin_uuid.create ~repr ~ml_bundles () in
    let trigger_unused_value_warnings_despite_mli =
      t.trigger_unused_value_warnings_despite_mli
    in
    Compile.copy_files
      ~trigger_unused_value_warnings_despite_mli
      ~working_dir
      plugin_uuid
    >>=? fun basename ->
    Compile.compile_and_load_file ~working_dir t ~export:false ~basename
    >>|? fun res -> plugin_uuid, res
  in
  Shell.absolute_pathnames filenames >>=? fun filenames ->
  Ml_bundle.from_filenames filenames >>=? fun ml_bundles ->
  match t.cache with
  | None -> compile_without_cache ml_bundles >>|? fun (_, (_, univ)) -> univ
  | Some cache ->
    Lazy_deferred.force_exn cache >>=? fun cache ->
    Plugin_cache.digest ml_bundles >>=? fun sources ->
    let refresh_cache () =
      compile_without_cache ml_bundles >>=? fun (plugin_uuid, (cmxs_filename, univ)) ->
      Plugin_cache.add cache sources plugin_uuid cmxs_filename >>|? fun () ->
      univ
    in
    match Plugin_cache.find cache sources with
    | Some plugin -> begin
      let cmxs_filename = Plugin_cache.Plugin.cmxs_filename plugin in
      Compile.dynlink cmxs_filename >>= function
      | Ok _ as ok -> Deferred.return ok
      | Error _ as error ->
        if Plugin_cache.old_cache_with_new_exec cache
        then
          (* In that case, since the exec has changed since the last time it was used to
             build this cache, we might have a chance that dynlinking a freshly rebuilt
             cmxs file would actually succeed.
             In the case where the plugin dynlinked normally but raises an exception at
             toplevel, we will go through this branch and recompile it a second time. It
             is probably fine though. *)
          refresh_cache ()
        else
          (* Rebuilding the cmxs from scratch would lead to the exact same file since we
             have the same exec that the one that was used to build the same
             sources. Thus, the result of the dynlink would the same anyway, something
             else should be wrong. *)
          Deferred.return error
    end
    | None -> refresh_cache ()
;;

exception Plugin_not_found of Plugin_uuid.t with sexp
exception Type_mismatch of string * string with sexp
;;

module type S = sig
  type t
  val load_ocaml_src_files :
    dynloader -> string list -> t Deferred.Or_error.t
  val check_ocaml_src_files :
    dynloader -> string list -> unit Deferred.Or_error.t
end
;;

module Make (X:Module_type) =
struct
  let load_ocaml_src_files_without_running_them t filenames =
    let repr = Plugin_uuid.Repr.create ~t:X.t_repr ~univ_constr:X.univ_constr_repr in
    load_ocaml_src_files_plugin_uuid ~repr t filenames
  ;;

  let load_ocaml_src_files t filenames =
    load_ocaml_src_files_without_running_them t filenames >>=? fun make_plugin ->
    (* There is an hidden invariant there: if the OCaml compilation succeed, that
       means that the loaded module has the type represented in X.repr, so the
       [Univ.match_] will succeed. Of course this is only true is the user gave
       a valid Module_type in the first place. *)
    let run () =
      try begin
        let univ = make_plugin () in
        match Univ.match_ univ X.univ_constr with
        | Some plugin -> Ok plugin
        | None ->
          Or_error.of_exn (Type_mismatch (X.t_repr, X.univ_constr_repr))
      end with exn ->
        Or_error.tag (Or_error.of_exn exn)
          "Exception while executing the plugin's toplevel"
    in
    match t.run_plugin_toplevel with
    | `In_async_thread  -> Deferred.return (run ())
    | `Outside_of_async -> In_thread.run run
  ;;

  let check_ocaml_src_files t filenames =
    load_ocaml_src_files_without_running_them t filenames >>|? fun (_ : unit -> Univ.t) ->
    ()
  ;;
end

module type Side_effect = sig
end

let side_effect_univ_constr = Univ_constr.create ()
;;

module Side_effect_loader = Make(struct
  type t = (module Side_effect)
  let t_repr = "Ocaml_plugin.Ocaml_dynloader.Side_effect"
  let univ_constr = side_effect_univ_constr
  let univ_constr_repr = "Ocaml_plugin.Ocaml_dynloader.side_effect_univ_constr"
end)

module Side_effect = struct
  let load_ocaml_src_files t filenames =
    Side_effect_loader.load_ocaml_src_files t filenames
    >>|? fun (_ : (module Side_effect)) -> ()
  ;;

  let check_ocaml_src_files = Side_effect_loader.check_ocaml_src_files
  ;;
end

open Core.Std
open Async.Std

(* The default policy about warnings *)
let default_warnings_spec = "@a-4-29"

let index = ref 0

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
  compilation_config : (string * Config.t option) Or_error.t Lazy_deferred.t;
  include_directories : string list;
  loaded : String.Hash_set.t;
  ocamlopt_opt : string;
  camlp4o_opt : string;
  pa_files : string list;
  cache : Plugin_cache.t Or_error.t Lazy_deferred.t option;
} with fields

let next_filename () =
  let s = Printf.sprintf "m_dyn_%d_.ml" !index in
  index := succ !index;
  s

let ocamlopt_opt = "ocamlopt.opt"
let camlp4o_opt = "camlp4o.opt"

module Compilation_config = struct

  let info_file_name = "info"
  let info_file dir = dir ^/ info_file_name

  module Info = struct
    (*
      save some debug info in the builddir in case this doesn't get cleaned
    *)

    type t = {
      login : string;
      hostname : string;
      pid : Pid.t;
      sys_argv : string array;
    } with sexp_of

    let create () =
      Shell.Deferred.Or_error.try_with ~extract_exn:true (fun () ->
        let hostname = Unix.gethostname () in
        let pid = Unix.getpid () in
        let sys_argv = Sys.argv in
        Unix.getlogin () >>| fun login ->
        let t = {
          login;
          hostname;
          pid;
          sys_argv;
        } in
        sexp_of_t t
      )

    let get = Lazy_deferred.create create

    let save info_file =
      Lazy_deferred.force_exn get >>=? fun info ->
        Shell.Deferred.Or_error.try_with ~extract_exn:true (fun () ->
          Writer.save_sexp ~hum:true info_file info
        )
  end

  let lazy_create ~initialize_compilation_callback ~in_dir =
    let compute () =
      Shell.temp_dir ~in_dir >>=? fun directory ->
      (match initialize_compilation_callback with
      | None -> return (Ok None)
      | Some task -> task directory) >>=? fun config_opt ->
      let info_file = info_file directory in
      Info.save info_file >>=? fun () ->
      return (Ok (directory, config_opt))
    in
    Lazy_deferred.create compute
end

let update_with_config t = function
  | None -> t
  | Some conf ->
    let pa_files = conf.Config.pa_files @ t.pa_files in
    { t with
      pa_files;
    }

exception Is_not_native with sexp

type 'a create_arguments =
  ?in_dir:string
  -> ?include_directories:string list
  -> ?custom_warnings_spec:string
  -> ?cmx_flags:string list
  -> ?cmxs_flags:string list
  -> ?use_cache:Plugin_cache.Config.t
  -> 'a

let create
    ?(in_dir = Filename.temp_dir_name)
    ?(include_directories = [])
    ?(custom_warnings_spec = default_warnings_spec)
    ?(cmx_flags = [])
    ?(cmxs_flags = [])
    ?use_cache
    ?initialize_compilation_callback
    ?(ocamlopt_opt = ocamlopt_opt)
    ?(camlp4o_opt = camlp4o_opt)
    ?(pa_files = [])
    ()
    =
  let cmx_flags =
    [ "-w" ; custom_warnings_spec ; "-warn-error" ; "+a" ]
    @ cmx_flags
  in
  if not Dynlink.is_native then Deferred.Or_error.of_exn Is_not_native else
  Shell.absolute_pathnames include_directories >>=? fun include_directories ->
  let loaded = String.Hash_set.create () in
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
      compilation_config;
      include_directories;
      loaded;
      ocamlopt_opt;
      camlp4o_opt;
      pa_files;
      cache;
    } in
  Deferred.return (Ok t)

let clean t =
  if not (Lazy_deferred.is_forced t.compilation_config) then return (Ok ())
  else begin
    Lazy_deferred.force_exn t.compilation_config >>=? fun (directory, _) ->
    let ext = [ "ml" ; "o" ; "cmx" ; "cmxs" ; "cmi" ] in
    let files =
      let fct file ext =
        let filename = Printf.sprintf "%s.%s" file ext in
        let filename = Filename.concat directory filename in
        filename
      in
      Hash_set.fold
        ~init:[]
        ~f:(fun acc file -> List.rev_map_append ext acc ~f:(fct file))
        t.loaded
    in
    let info_file = Compilation_config.info_file directory in
    Shell.rm ~f:() (info_file :: files) >>=? fun () ->
    (* try to clean the directory at the end if it is not empty *)
    Shell.rmdir directory >>| fun result ->
    t.cleaned <- true;
    Ok (ignore (result : unit Or_error.t))
  end

module type Module_type =
sig
  type t
  val t_repr : string
  val univ_constr : t Univ.Constr.t
  val univ_constr_repr : string
end

exception No_file_to_compile with sexp
exception Dynlink_error of string with sexp

module Compile : sig

  val copy_files : working_dir:string -> Plugin_uuid.t -> string Deferred.Or_error.t

  val dynlink : ?export:bool -> string -> unit Deferred.Or_error.t

  val compile_and_load_file :
    working_dir:string
    -> t
    -> ?export:bool
    -> basename:Core.Std.String.Hash_set.elt
    -> string Async.Std.Deferred.Or_error.t

end = struct

  let output_in_channel out_channel in_channel =
    let content = In_channel.input_all in_channel in
    Out_channel.output_string out_channel content

  let add_position_directive out_channel filename =
    Out_channel.output_string out_channel (Printf.sprintf "#1 %S\n" filename)

  let permission = 0o600

  let copy_files ~working_dir plugin_uuid =
    let fct () =
      let with_bundle ~typed ~plugin_uuid out_channel bundle =
        let module_name, filename, intf_filename_opt =
          let get_module_name path =
            let basename = Filename.basename path in
            let name = Filename.chop_extension basename (* we are assured that this won't
            fail because [Ml_bundle.to_pathnames] always returns absolute paths with
            extensions. *)
            in String.capitalize name
          in match Ml_bundle.to_pathnames bundle with
          | `pair (`ml ml_path, `mli mli_path) ->
             get_module_name ml_path, ml_path, Some mli_path
          | `ml ml_path -> get_module_name ml_path, ml_path, None
        in
        Out_channel.output_string out_channel (
          Plugin_table.Generation_helper.header_code
            ~typed ~plugin_uuid ~module_name ~has_mli:(Option.is_some intf_filename_opt)
        );
        begin match intf_filename_opt with
        | None -> () (* do nothing *)
        | Some intf_filename ->
          add_position_directive out_channel intf_filename;
          In_channel.with_file ~binary:false intf_filename
            ~f:(output_in_channel out_channel);
          Out_channel.output_string out_channel
            Plugin_table.Generation_helper.sig_struct_separation
        end ;
        add_position_directive out_channel filename;
        In_channel.with_file ~binary:false filename ~f:(output_in_channel out_channel);
        Out_channel.output_string out_channel
          (Plugin_table.Generation_helper.trailer_code ~typed ~plugin_uuid ~module_name);
      in
      let target = next_filename () in
      let full_target = working_dir ^/ target in
      Out_channel.with_file ~binary:false ~perm:permission full_target
        ~f:(fun out_channel ->
          let utils, last =
            match List.rev (Plugin_uuid.ml_bundles plugin_uuid) with
            | [] -> raise No_file_to_compile
            | last :: utils -> List.rev utils, last
          in
          Out_channel.output_string out_channel
            (Plugin_table.Generation_helper.let_plugin_uuid ~plugin_uuid);
          List.iter utils ~f:(with_bundle ~typed:false ~plugin_uuid out_channel);
          with_bundle ~typed:true ~plugin_uuid out_channel last
        );
      target
    in
    Shell.Deferred.Or_error.try_with ~extract_exn:true (fun () -> In_thread.run fct)

  let blocking_dynlink ?(export=false) file =
    let loadfile = if export then Dynlink.loadfile else Dynlink.loadfile_private in
    try
      loadfile file
    with
    | Dynlink.Error e -> raise (Dynlink_error (Dynlink.error_message e))

  let dynlink ?export cmxs_filename =
    let fct () = blocking_dynlink ?export cmxs_filename in
    Shell.Deferred.Or_error.try_with ~extract_exn:true (fun () -> In_thread.run fct)

  let compile_and_load_file ~working_dir t ?export ~basename =
    let chop_extension =
      try Filename.chop_extension basename
      with Invalid_argument _ -> basename
    in
    Hash_set.add t.loaded chop_extension;
    let ext  = Printf.sprintf "%s.%s" chop_extension in
    let ml   = ext "ml" in
    let cmx  = ext "cmx" in
    let cmxs = ext "cmxs" in
    let include_directories =
      let f dir = [ "-I" ; dir ] in
      List.concat_map ~f t.include_directories
    in
    let pp_args = match t.pa_files with
      | [] -> []
      | cmxs -> [ "-pp" ; String.concat (t.camlp4o_opt::cmxs) ~sep:" " ]
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
    dynlink ?export cmxs >>|? fun () ->
    cmxs
end

exception Usage_of_cleaned_dynloader with sexp

let load_ocaml_src_files_plugin_uuid ~repr t filenames =
  if t.cleaned then Deferred.Or_error.of_exn Usage_of_cleaned_dynloader else
  let compile_without_cache ml_bundles =
    Lazy_deferred.force_exn t.compilation_config >>=? fun (working_dir, config_opt) ->
    let t = update_with_config t config_opt in
    let plugin_uuid = Plugin_uuid.create ~repr ~ml_bundles () in
    Compile.copy_files ~working_dir plugin_uuid >>=? fun basename ->
    Compile.compile_and_load_file ~working_dir t ~export:false ~basename
    >>|? fun cmxs_filename ->
    plugin_uuid, cmxs_filename
  in
  Shell.absolute_pathnames filenames >>=? fun filenames ->
  Ml_bundle.from_filenames filenames >>=? fun ml_bundles ->
  match t.cache with
  | None -> compile_without_cache ml_bundles >>|? fun (plugin_uuid, _) -> plugin_uuid
  | Some cache ->
    Lazy_deferred.force_exn cache >>=? fun cache ->
    Plugin_cache.digest ml_bundles >>=? fun sources ->
    let refresh_cache () =
      compile_without_cache ml_bundles >>=? fun (plugin_uuid, cmxs_filename) ->
      Plugin_cache.add cache sources plugin_uuid cmxs_filename >>|? fun () ->
      plugin_uuid
    in
    match Plugin_cache.find cache sources with
    | Some plugin -> begin
      let cmxs_filename = Plugin_cache.Plugin.cmxs_filename plugin in
      Compile.dynlink cmxs_filename >>= function
      | Ok () ->
        Deferred.Or_error.return (Plugin_cache.Plugin.plugin_uuid plugin)
      | Error _ as error ->
        if Plugin_cache.old_cache_with_new_exec cache
        then
          (* in that case, since the exec has changed since the last time it was used to
             build this cache, we might have a chance that dynlinking a freshly rebuilt
             cmxs file would actually succeed *)
          refresh_cache ()
        else
          (* rebuilding the cmxs from scratch would lead to the exact same file since we
             have the same exec that the one that was used to build the same
             sources. Thus, the result of the dynlink would the same anyway, something
             else should be wrong. *)
          Deferred.return error
    end
    | None -> refresh_cache ()

exception Plugin_not_found of Plugin_uuid.t with sexp
exception Type_mismatch of string * string with sexp

module Make (X:Module_type) =
struct
  let load_ocaml_src_files t filenames =
    let repr = Plugin_uuid.Repr.create ~t:X.t_repr ~univ_constr:X.univ_constr_repr in
    load_ocaml_src_files_plugin_uuid ~repr:(Some repr)
      t filenames >>| fun result -> Or_error.bind result (fun plugin_uuid ->
        match Plugin_table.find_and_remove ~plugin_uuid () with
        | None -> Or_error.of_exn (Plugin_not_found plugin_uuid)
        | Some black -> (
        (*
          There is an hidden invariant there:
          if the OCaml compilation succeed, that means that the loaded module
          has the type represented in X.repr, so the [Univ.match_] will succeed
        *)
        match Univ.match_ black X.univ_constr with
        | Some t -> Ok t
        | None ->
          Or_error.of_exn (Type_mismatch (X.t_repr, X.univ_constr_repr))
      ))
end

let load_ocaml_src_files t filenames =
  load_ocaml_src_files_plugin_uuid ~repr:None t filenames
  >>|? fun (_ : Plugin_uuid.t) ->
  ()

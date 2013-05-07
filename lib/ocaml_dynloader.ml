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
  ocamlopt_opt : string;
  camlp4o_opt : string;
  pa_files : string list; (* Usually contains explicit provided preprocessors. The ones
                             from the config will be added right before generating the
                             command lines *)
  cache : Plugin_cache.t Or_error.t Lazy_deferred.t option;
} with fields

let next_filename () =
  let s = Printf.sprintf "m_dyn_%d_.ml" !index in
  incr index;
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
      version : string;
      build_info : string;
    } with sexp_of

    let create () =
      Shell.Deferred.Or_error.try_with ~extract_exn:true (fun () ->
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
      ocamlopt_opt;
      camlp4o_opt;
      pa_files;
      cache;
    } in
  Deferred.return (Ok t)

let clean t =
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
      Shell.rm ~r:() ~f:() [directory] >>| fun result ->
      t.cleaned <- true;
      result
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

  val dynlink :
    ?export:bool
    -> plugin_uuid:Plugin_uuid.t
    -> string -> unit Deferred.Or_error.t

  val compile_and_load_file :
    working_dir:string
    -> plugin_uuid:Plugin_uuid.t
    -> t
    -> ?export:bool
    -> basename:Core.Std.String.Hash_set.elt
    -> string Async.Std.Deferred.Or_error.t

end = struct

  let output_in_channel out_channel in_channel =
    let content = In_channel.input_all in_channel in
    Out_channel.output_string out_channel content

  let permission = 0o600

  let copy_files ~working_dir plugin_uuid =
    let fct () =
      let repr_opt = Plugin_uuid.repr plugin_uuid in
      let with_bundle ~last_module out_channel bundle =
        let `ml filename, `mli intf_filename_opt, `module_name module_name =
          Ml_bundle.to_pathnames bundle
        in
        Printf.fprintf out_channel "module %s " module_name;
        begin match intf_filename_opt with
        | None ->
          begin match repr_opt with
          | Some repr when last_module ->
            Printf.fprintf out_channel ": %s = struct\n" (Plugin_uuid.Repr.t repr)
          | _ ->
            Printf.fprintf out_channel "= struct\n"
          end
        | Some intf_filename ->
          Printf.fprintf out_channel " : sig\n";
          Printf.fprintf out_channel "#1 %S\n" intf_filename;
          In_channel.with_file ~binary:false intf_filename
            ~f:(output_in_channel out_channel);
          Printf.fprintf out_channel "\nend = struct\n";
        end;
        Printf.fprintf out_channel "#1 %S\n" filename;
        In_channel.with_file ~binary:false filename ~f:(output_in_channel out_channel);
        Printf.fprintf out_channel "\nend\n";
        if last_module then begin
          match repr_opt with
          | None -> ()
          | Some repr ->
            Printf.fprintf out_channel (
              "let () =\n" ^^
              "  let module P = Ocaml_plugin.Plugin_table in\n" ^^
              "  P.register ~plugin_uuid %s (module %s : %s)\n"
            ) (Plugin_uuid.Repr.univ_constr repr) module_name (Plugin_uuid.Repr.t repr)
        end
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
          Printf.fprintf out_channel
            "let plugin_uuid = Ocaml_plugin.Plugin_uuid.t_of_string %S\n"
            (Plugin_uuid.string_of_t plugin_uuid);
          List.iter utils ~f:(with_bundle ~last_module:false out_channel);
          with_bundle ~last_module:true out_channel last;

        );
      target
    in
    Shell.Deferred.Or_error.try_with ~extract_exn:true (fun () -> In_thread.run fct)


  (* Dynlink has the following not really wanted property: dynlinking a file with a given
     filename only works properly the first time. Further dynlinks with the same filename
     (even a different file) will not load the new module but instead execute the initial
     module. Since ocaml_plugin need to be able to load cmxs coming from ml files with the
     same name (several variations of config.ml for instance), what we do is give unique
     name to each cmxs that we produce: files in the cache have their uuid in the name,
     and files not in the cache are called $tmp_dir/something_$fresh.cmxs.
     We can't have several Ocaml_dynloader.t use the same directory, because ocaml_plugin
     always create a fresh directory in which to put its files. *)
  let blocking_dynlink ?(export=false) ~plugin_uuid file =
    let loadfile = if export then Dynlink.loadfile else Dynlink.loadfile_private in
    try
      loadfile file
    with
    | Dynlink.Error e ->
      Plugin_table.remove ~plugin_uuid;
      raise (Dynlink_error (Dynlink.error_message e))

  let dynlink ?export ~plugin_uuid cmxs_filename =
    let fct () = blocking_dynlink ?export ~plugin_uuid cmxs_filename in
    Shell.Deferred.Or_error.try_with ~extract_exn:true (fun () -> In_thread.run fct)

  let compile_and_load_file ~working_dir ~plugin_uuid t ?export ~basename =
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
        [ "-pp" ; String.concat (t.camlp4o_opt::cmxs) ~sep:" " ]
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
    dynlink ?export ~plugin_uuid cmxs >>|? fun () ->
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
    Compile.compile_and_load_file ~working_dir ~plugin_uuid t ~export:false ~basename
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
      let plugin_uuid = Plugin_cache.Plugin.plugin_uuid plugin in
      Compile.dynlink ~plugin_uuid cmxs_filename >>= function
      | Ok () ->
        Deferred.Or_error.return plugin_uuid
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

exception Plugin_not_found of Plugin_uuid.t with sexp
exception Type_mismatch of string * string with sexp

module Make (X:Module_type) =
struct
  let load_ocaml_src_files t filenames =
    let repr = Plugin_uuid.Repr.create ~t:X.t_repr ~univ_constr:X.univ_constr_repr in
    load_ocaml_src_files_plugin_uuid ~repr:(Some repr) t filenames >>| fun result ->
    Or_error.bind result (fun plugin_uuid ->
      match Plugin_table.find_and_remove ~plugin_uuid () with
      | None -> Or_error.of_exn (Plugin_not_found plugin_uuid)
      | Some black -> (
          (* There is an hidden invariant there: if the OCaml compilation succeed, that
             means that the loaded module has the type represented in X.repr, so the
             [Univ.match_] will succeed. Of course this is only true is the user gave
             a valid Module_type in the first place. *)
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

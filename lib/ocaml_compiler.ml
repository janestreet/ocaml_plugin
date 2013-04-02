open Core.Std
open Async.Std

let section_id = "dynlink"
let tar_id = section_id ^ ".tgz"

let ocamlopt_opt = Ocaml_dynloader.ocamlopt_opt
let camlp4o_opt = Ocaml_dynloader.camlp4o_opt

let pervasives = "pervasives.cmi"
let config_file = "config.sexp"

let mandatory_embedded_files = [
  ocamlopt_opt;
  pervasives;
]

module Directory = struct
  type t = {
    (* invariant here: directory is always an absolute pathname *)
    working_dir : string;
    files : string list;
  } with fields
end

type t = {
  loader : Ocaml_dynloader.t;
  initialize : Directory.t Or_error.t Lazy_deferred.t;
} with fields

let force_initialize (type a) t (with_dir : Directory.t -> a) : a Deferred.Or_error.t =
  Lazy_deferred.force_exn t.initialize >>|? with_dir

let files t = force_initialize t Directory.files

let directory t = force_initialize t Directory.working_dir

let directory_files t =
  let with_dir t =
    Directory.working_dir t, Directory.files t
  in
  force_initialize t with_dir

let read_directory t =
  Lazy_deferred.wait_exn t.initialize >>|? Directory.working_dir

let clean t =
  if not (Lazy_deferred.is_forced t.initialize) then Deferred.return (Ok ())
  else
    directory t >>=? fun working_dir ->
    Shell.rm ~r:() ~f:() [ working_dir ]

let force t = force_initialize t ignore

let get_code_location pid =
  let pid = Int.to_string (Pid.to_int pid) in
  let link = "/proc" ^/ pid ^/ "exe" in
  link

(* This returns a Deferred.t because the implementation may need to interact
   with the file system.
   Although the current implementation doesn't, we do not want to change
   the interface if this interaction becomes mandatory at some point. *)
let get_my_code_file () =
  let pid = Unix.getpid () in
  Deferred.return (get_code_location pid)

exception Mandatory_file_not_found of string * string list with sexp

(*
  This function returns a Deferred.t because at some point we want to add additional
  checks over the mandatory files (like calling ocamlopt.opt on a dummy ml file,
  or things like that. We do not want to change the interface of that function
  once we will add these checks.
*)
let check_mandatory_files files =
  let fct () =
    let iter file =
      if not (List.mem ~equal:String.equal files file)
      then raise (Mandatory_file_not_found (file, files))
    in
    List.iter ~f:iter mandatory_embedded_files;
    (* here we will add additional checks over the mandatory files and these
       checks will need some I/O *)
    Deferred.return ()
  in
  Shell.Deferred.Or_error.try_with ~extract_exn:true fct

type 'a create_arguments =
  ?code_file : [`my_code | `file of string]
  -> 'a Ocaml_dynloader.create_arguments

let create
    ?code_file
    ?in_dir
    ?include_directories
    ?custom_warnings_spec
    ?cmx_flags
    ?cmxs_flags
    ?use_cache
    () =
  let initialize = ref None in
  let initialize_compilation_callback ~directory:working_dir =
    let get_compilation_config working_dir =
      Shell.Deferred.Or_error.try_with ~extract_exn:true (fun () ->
        Reader.load_sexp_exn (working_dir ^/ config_file) Ocaml_dynloader.Config.t_of_sexp
      )
    in
    let deferred =
      (match code_file with
      | None | Some `my_code -> get_my_code_file ()
      | Some (`file filename) -> Deferred.return filename
      ) >>= fun filename ->
      let destination = working_dir ^/ tar_id in
      Objcopy.retrieve ~filename ~section_id ~destination >>=? fun () ->
      Tar.extract ~working_dir destination >>=? fun files ->
      check_mandatory_files files >>= function
      | Error exn ->
        Shell.rm ~r:() ~f:() [ working_dir ] >>=? fun () ->
        Deferred.return (Error exn)
      | Ok () ->
        let directory = {
          Directory.
          working_dir;
          files;
        } in
        Deferred.return (Ok directory)
    in
    initialize := Some deferred;
    deferred >>=? fun dir ->
    if List.mem dir.Directory.files config_file
    then get_compilation_config dir.Directory.working_dir >>|? Option.some
    else Deferred.return (Ok None)
  in
  let ocamlopt_opt = "." ^/ ocamlopt_opt in
  let camlp4o_opt = "." ^/ camlp4o_opt in
  let nostdlib flags = "-nostdlib" :: Option.value ~default:[] flags in
  let cmx_flags = nostdlib cmx_flags in
  let cmxs_flags = nostdlib cmxs_flags in
  Ocaml_dynloader.create
    ?in_dir
    ?include_directories
    ?custom_warnings_spec
    ~cmx_flags
    ~cmxs_flags
    ?use_cache
    ~initialize_compilation_callback
    ~ocamlopt_opt
    ~camlp4o_opt
    () >>=? fun loader ->
  let initialize =
    let def = Ocaml_dynloader.compilation_config loader in
    Lazy_deferred.follow def (function
    | Error _ as error -> Deferred.return error
    | Ok _ ->
      match !initialize with
      | None -> assert false
      | Some def -> def
    )
  in
  let compiler = {
    loader;
    initialize;
  } in
  Deferred.return (Ok (`this_needs_manual_cleaning_after compiler))

let with_compiler
    ?code_file
    ?in_dir
    ?include_directories
    ?custom_warnings_spec
    ?cmx_flags
    ?cmxs_flags
    ?use_cache
    ()
    ~f
    =
  create
    ?code_file
    ?in_dir
    ?include_directories
    ?custom_warnings_spec
    ?cmx_flags
    ?cmxs_flags
    ?use_cache
    ()
    >>=? function `this_needs_manual_cleaning_after compiler ->
  Shell.Deferred.Or_error.try_with_join ~extract_exn:true (fun () -> f compiler)
  >>= fun result ->
  clean compiler >>=? fun () ->
  Deferred.return result

let make_load_ocaml_src_files load_ocaml_src_files =
  let aux
      ?code_file
      ?in_dir
      ?include_directories
      ?custom_warnings_spec
      ?cmx_flags
      ?cmxs_flags
      ?use_cache
      files =
    let f compiler =
      let loader = loader compiler in
      load_ocaml_src_files loader files
    in
    with_compiler
      ?code_file
      ?in_dir
      ?include_directories
      ?custom_warnings_spec
      ?cmx_flags
      ?cmxs_flags
      ?use_cache
      ()
      ~f
  in
  aux

module Make (X:Ocaml_dynloader.Module_type) = struct
  module M = Ocaml_dynloader.Make(X)
  let load_ocaml_src_files = make_load_ocaml_src_files M.load_ocaml_src_files
end

let load_ocaml_src_files = make_load_ocaml_src_files Ocaml_dynloader.load_ocaml_src_files

open Core
open Async

type t =
  { ml          : string
  ; mli         : string option
  ; module_name : string
  }
[@@deriving sexp, compare] [@@sexp.allow_extra_fields]

type tmp_t =
  { mutable tmp_ml  : string option
  ; mutable tmp_mli : string option
  ; tmp_module_name : string
  }

let valid_module_name s =
  s <> "" &&
  match s.[0] with
  | 'A'..'Z' ->
    String.for_all s ~f:(function
      | 'a'..'z' | 'A'..'Z' | '_' | '0'..'9' | '\'' -> true
      | _ -> false
    )
  | _ -> false
;;

let module_name ~full_path ~path_no_ext =
  let basename = Filename.basename path_no_ext in
  let unchecked_module_name = String.capitalize basename in
  if valid_module_name unchecked_module_name then
    unchecked_module_name
  else
    invalid_argf "%s is not a valid ocaml filename" full_path ()
;;

let enrich_bundle ({ ml; mli; module_name = _ } as bundle) =
  match mli with
  | Some _ -> return bundle
  | None ->
    let mli = Filename.chop_extension ml ^ ".mli" in
    Sys.file_exists mli >>| function
    | `Yes -> { bundle with mli = Some mli }
    | `No -> bundle
    | `Unknown -> raise_s [%sexp "File_in_unknown_state", (mli : string), [%here]]
;;

let ml_with_mli_reorder filenames =
  let tbl = String.Table.create () in
  let init_bundle acc str =
    let path_no_ext, ext_opt = Filename.split_extension str in
    let ext =
      match ext_opt with
      | None -> `none
      | Some "ml" -> `ml
      | Some "mli" -> `mli
      | Some ext -> invalid_argf "Expected .ml or .mli files, got : %s" ext ()
    in
    (* giving this error after the one about extensions *)
    let module_name = module_name ~full_path:str ~path_no_ext in
    let acc, data =
      match Hashtbl.find tbl module_name with
      | None ->
        let data = { tmp_ml = None; tmp_mli = None; tmp_module_name = module_name } in
        Hashtbl.add_exn tbl ~key:module_name ~data;
        data :: acc, data
      | Some data -> acc, data
    in
    begin match ext, data with
    | (`ml | `none), { tmp_ml = Some old_ml; _ } ->
      invalid_argf "Several implementations provided for %s: %s and %s"
        module_name str old_ml ()
    | `mli, { tmp_mli = Some old_mli; _ } ->
      invalid_argf "Several interfaces provided for %s: %s and %s"
        module_name str old_mli ()
    | `none , { tmp_ml = None; _ } ->
      data.tmp_ml <- Some (str ^ ".ml")
    | `ml, { tmp_ml = None; _ } ->
      data.tmp_ml <- Some str
    | `mli, { tmp_mli = None; _ } ->
      data.tmp_mli <- Some str
    end;
    acc
  in
  let rev_paths = List.fold_left filenames ~init:[] ~f:init_bundle in
  List.rev_map rev_paths ~f:(
    fun { tmp_ml; tmp_mli = mli; tmp_module_name = module_name } ->
      let ml =
        match tmp_ml with
        | None ->
          (* same behaviour as before *)
          Filename.chop_extension (Option.value_exn mli) ^ ".ml"
        | Some ml -> ml
      in
      { ml; mli; module_name }
  )
;;

let from_filenames filenames =
  Deferred.Or_error.try_with ~extract_exn:true (fun () ->
    let pairs = ml_with_mli_reorder filenames in
    Deferred.List.map pairs ~f:enrich_bundle
  )
;;

let to_pathnames { ml; mli; module_name } =
  `ml ml, `mli mli, `module_name module_name
;;

let module_name t = t.module_name
;;

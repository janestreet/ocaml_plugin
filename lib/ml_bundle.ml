open Core.Std
open Async.Std

type t = string * bool with sexp, bin_io, compare

exception File_in_unknow_state of string with sexp

let enrich_bundle ((name, has_mli) as bundle) =
  if has_mli then
    return bundle
  else
    let mli = sprintf "%s.mli" name in
    Sys.file_exists mli >>| function
    | `Yes -> (name, true)
    | `No -> (name, false)
    | `Unknown -> raise (File_in_unknow_state mli)

let ml_with_mli_reorder filenames =
  let mli_set = String.Hash_set.create () in
  let global_set = String.Hash_set.create () in
  let init_bundle acc str =
    let path, ext_opt = Filename.split_extension str in
    begin match ext_opt with
    | None | Some "ml" -> () (* do nothing *)
    | Some "mli" -> Hash_set.add mli_set path
    | Some ext -> invalid_argf "Expected .ml or .mli files, got : %s" ext ()
    end ;
    if Hash_set.mem global_set path then
      acc (* do nothing *)
    else begin
      Hash_set.add global_set path ;
      path :: acc
    end
  in
  let paths = List.fold_left filenames ~init:[] ~f:init_bundle in
  List.rev_map paths ~f:(fun path -> (path, Hash_set.mem mli_set path))

let from_filenames filenames =
  Shell.Deferred.Or_error.try_with ~extract_exn:true (fun () ->
    let pairs = ml_with_mli_reorder filenames in
    Deferred.List.map pairs ~f:enrich_bundle
  )

let to_ml file = `ml (file ^ ".ml")
let to_mli file = `mli (file ^ ".mli")

let to_pathnames (name, has_mli) =
  if has_mli then
    `pair (to_ml name, to_mli name)
  else
    to_ml name

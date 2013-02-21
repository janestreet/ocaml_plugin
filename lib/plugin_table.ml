open Core.Std

type t = Univ.t

let table = (Plugin_uuid.Table.create () : t Plugin_uuid.Table.t)

let reset () = Plugin_uuid.Table.clear table

let find_and_remove ~plugin_uuid () =
  match Plugin_uuid.Table.find table plugin_uuid with
  | None -> None
  | (Some _) as black ->
    Plugin_uuid.Table.remove table plugin_uuid;
    black

let register ~plugin_uuid constr black =
  let univ = Univ.create constr black in
  Plugin_uuid.Table.replace ~key:plugin_uuid ~data:univ table

module Generation_helper = struct

  let let_plugin_uuid ~plugin_uuid =
    Printf.sprintf "\
let plugin_uuid = Ocaml_plugin.Plugin_uuid.t_of_string %S
"
      (Plugin_uuid.string_of_t plugin_uuid)

  let header_without_mli ~module_name repr_opt =
    let sig_str = match repr_opt with
      | None -> ""
      | Some repr -> Printf.sprintf ": %s " (Plugin_uuid.Repr.t repr)
    in
    Printf.sprintf "module %s %s= struct" module_name sig_str

  let header_with_mli ~module_name = Printf.sprintf "module %s : sig\n" module_name

  let sig_struct_separation = "\nend = struct\n"

  let unit_trailer_code ~module_name:_ = "\
end
"

  let sig_trailer_code ~module_name ~repr =
    let intf = Plugin_uuid.Repr.t repr in
    let univ_constr_repr = Plugin_uuid.Repr.univ_constr repr in
    Printf.sprintf "\
end
let () =
  let module P = Ocaml_plugin.Plugin_table in
  P.register ~plugin_uuid %s (module %s : %s)
"
      univ_constr_repr
      module_name intf


  let header_code ?(typed=true) ~plugin_uuid ~module_name ~has_mli =
    if has_mli then
      header_with_mli ~module_name
    else if typed then
      header_without_mli ~module_name (Plugin_uuid.repr plugin_uuid)
    else
      header_without_mli ~module_name None

  let trailer_code ?(typed=true) ~plugin_uuid ~module_name =
    match typed, Plugin_uuid.repr plugin_uuid with
    | true, Some repr -> sig_trailer_code ~module_name ~repr
    | _ -> unit_trailer_code ~module_name
end

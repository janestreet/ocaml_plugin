open Core.Std

let table = (Plugin_uuid.Table.create () : Univ.t Plugin_uuid.Table.t)

let remove ~plugin_uuid =
  Plugin_uuid.Table.remove table plugin_uuid

let find_and_remove ~plugin_uuid () =
  match Plugin_uuid.Table.find table plugin_uuid with
  | None -> None
  | (Some _) as black ->
    Plugin_uuid.Table.remove table plugin_uuid;
    black

let register ~plugin_uuid constr black =
  let univ = Univ.create constr black in
  Plugin_uuid.Table.replace ~key:plugin_uuid ~data:univ table

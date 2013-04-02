open Core.Std

val remove : plugin_uuid:Plugin_uuid.t -> unit
val find_and_remove : plugin_uuid:Plugin_uuid.t -> unit -> Univ.t option
val register : plugin_uuid:Plugin_uuid.t -> 'a Univ.Constr.t -> 'a -> unit

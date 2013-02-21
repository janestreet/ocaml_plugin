open Core.Std

type t = Univ.t

val find_and_remove : plugin_uuid:Plugin_uuid.t -> unit -> t option
val register : plugin_uuid:Plugin_uuid.t -> 'a Univ.Constr.t -> 'a -> unit

val reset : unit -> unit

module Generation_helper : sig
  val let_plugin_uuid : plugin_uuid:Plugin_uuid.t -> string
  val sig_struct_separation : string
  val header_code :
    ?typed:bool
    -> plugin_uuid:Plugin_uuid.t -> module_name:string -> has_mli:bool -> string
  val trailer_code :
    ?typed:bool
    -> plugin_uuid:Plugin_uuid.t -> module_name:string -> string
end

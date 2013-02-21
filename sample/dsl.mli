open Core.Std

type t1
val make_v1 : string -> t1

module type Config_intf_v1 =
sig
  val job : t1
end

val univ_constr_v1 : (module Config_intf_v1) Univ.Constr.t

type t2
val make_v2 : string -> t2

module type Config_intf_v2 =
sig
  val job : t2
end

val univ_constr_v2 : (module Config_intf_v2) Univ.Constr.t

val register_v1 : (module Config_intf_v1) -> unit
val register_v2 : (module Config_intf_v2) -> unit

(**
   do something with the registered jobs
*)
val exec : unit -> unit


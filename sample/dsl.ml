open Core.Std

type t1 = string
let make_v1 s = s

module type Config_intf_v1 =
sig
  val job : t1
end

let univ_constr_v1 =
  (Univ.Constr.create "Dsl.Config_intf_v1" sexp_of_opaque :
     (module Config_intf_v1) Univ.Constr.t)

type t2 = string
let make_v2 s = s

module type Config_intf_v2 =
sig
  val job : t2
end

let univ_constr_v2 =
  (Univ.Constr.create "Dsl.Config_intf_v2" sexp_of_opaque :
     (module Config_intf_v2) Univ.Constr.t)

let table_v1 = ref ([] : (module Config_intf_v1) list)
let register_v1 m = table_v1 := m :: !table_v1

let table_v2 = ref ([] : (module Config_intf_v2) list)
let register_v2 m = table_v2 := m :: !table_v2

let print_m1 m =
  let module M = (val m : Config_intf_v1) in
  print_endline M.job

let print_m2 m =
  let module M = (val m : Config_intf_v2) in
  print_endline M.job

let exec () =
  Printf.printf "Starting the configured runtime\n%!";
  List.iter ~f:print_m1 !table_v1;
  List.iter ~f:print_m2 !table_v2;
  Printf.printf "ending the runtime\n%!"

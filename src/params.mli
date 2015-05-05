open Core.Std

(** this file is replaced by an alternative file in the release version, since
    Version_util is not released and therefore not available. *)
(* what we actually use from version util *)
val build_info_as_sexp   : Sexp.t
val version              : string

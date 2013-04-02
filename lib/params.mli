open Core.Std

(** this file is replaced by an alternative file in the release version, since
    Version_util is not released and therefore not available. In this alternative version,
    [build_info_available = false] and the versions are dummy and constant strings, not
    used in the logic of the lib *)
val build_info_available : bool

(* what we actually use from version util *)
val build_info_as_sexp   : Sexp.t
val version              : string
val build_info           : string

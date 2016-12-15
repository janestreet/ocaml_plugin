open! Core.Std

(** This file is replaced by an alternative file in the release version, since
    [Version_util] is not released and therefore not available. *)
(** What we actually use from [Version_util]. *)
val build_info_as_sexp   : Sexp.t
val version              : string

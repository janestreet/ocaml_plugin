open Core.Std

type t [@@deriving sexp]

val sexped : Sexp.t

val first : t


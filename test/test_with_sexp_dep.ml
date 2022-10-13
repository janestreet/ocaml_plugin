open! Core

type t =
  { foo : float option
  ; bar : Test_with_sexp.t
  }
[@@deriving sexp]

let sexped = sexp_of_t { foo = Some 3.; bar = Test_with_sexp.first }
let first = t_of_sexp sexped

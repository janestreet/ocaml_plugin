open Core.Std

type t = {
  foo : int ;
  bar : string ;
} with sexp

let sexped = sexp_of_t { foo = 1 ; bar = "toto" }

let first = t_of_sexp sexped

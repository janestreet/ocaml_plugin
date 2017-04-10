open Core

type t = {
  x : int list;
} [@@deriving sexp, fields]

let () = assert ({x = [1]} = [%of_sexp: t] (Sexp.of_string "((x (1)))"))
let () = assert (x {x = [1]} = [1])

let message = "Hey, turns out you don't need to write sexpifiers by hand. Awesome!!"


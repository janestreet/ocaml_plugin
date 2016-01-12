open Core.Std
open Async.Std

type t [@@deriving sexp, compare]

(* The argument is a list of absolute paths (with or without extensions) to ml
   and mli files. The order is such that it is stable regarding the first file found
   in the input list regardless if it is an ml or mli, or without extension *)
val from_filenames : string list -> t list Deferred.Or_error.t

(* The string parameters in the result are absolute paths. *)
val to_pathnames : t ->
  [ `ml of string ] * [ `mli of string option ] * [ `module_name of string ]

val module_name : t -> string

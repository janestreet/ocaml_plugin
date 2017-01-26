open! Core
open! Async.Std

(** Call [ocamldep] to recursively find dependencies for the [target] compilation unit,
    return a list of compilation units the [target] depends on in topological order.

    Failure cases includes:
    - cannot run ocamldep.opt
    - invalid working_dir, target not found
    - ocamldep errors (the program crashes, syntax errors, etc.)
    - cyclic dependency detected

    A compilation unit is a string representing an ml/mli module.  It does not contain the
    extension.  Example:

    {v
       file              compilation unit
       a.mli?            "a"
       Stack.mli?        "Stack"
       /path/to/file.ml  "file"
       /other/file.ml    "file"
    v}

    This function assumes that the target and all its dependencies are located under the
    same directory, that is [working_dir], and that an .ml and an .mli for the same module
    have the same case. *)
val find_dependencies
  :  prog:string (* path to ocamldep.opt *)
  -> args:string list (* extra arguments for ocamldep.opt *)
  -> working_dir:string (* the dir target is in *)
  -> target:string (* name of target compilation unit *)
  -> string list Deferred.Or_error.t


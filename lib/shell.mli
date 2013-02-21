open Core.Std
open Async.Std


module Deferred : sig
  module Or_error : sig
    type 'a t = 'a Deferred.Or_error.t
    val try_with :
      ?extract_exn:bool -> ?name:string -> (unit -> 'a Deferred.t)
      -> 'a Deferred.Or_error.t

    val try_with_join :
      ?extract_exn:bool -> ?name:string -> (unit -> 'a Deferred.Or_error.t)
      -> 'a Deferred.Or_error.t
  end
end

(*
  Global properties regarding bash sys calls. Echo would print the command before running
  them, and verbose would print the full output of the command.
*)
val set_defaults : ?verbose:bool -> ?echo:bool -> unit -> unit

(**
   Offers a command line spec to set verbose and echo directly.
*)
val flags : unit -> ('a, 'a) Command.Spec.t

val run :
  ?working_dir:string
  -> ?quiet_or_error:bool
  -> string
  -> string list
  -> unit Deferred.Or_error.t

val run_lines :
  ?working_dir:string
  -> string
  -> string list
  -> string list Deferred.Or_error.t

val mkdir_p : ?perm:int -> string -> unit Deferred.Or_error.t

val getcwd : unit -> string Deferred.Or_error.t

val chmod : string -> perm:Unix.file_perm -> unit Deferred.Or_error.t

(**
   When this function succeed, it returns a absolute pathname.
*)
val temp_dir : in_dir:string -> string Deferred.Or_error.t

val absolute_pathname : string -> string Deferred.Or_error.t
val absolute_pathnames : string list -> string list Deferred.Or_error.t

val rm : ?r:unit -> ?f:unit -> string list -> unit Deferred.Or_error.t
val rmdir : string -> unit Deferred.Or_error.t
val cp : source:string -> dest:string -> unit Deferred.Or_error.t

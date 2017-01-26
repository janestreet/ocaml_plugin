open! Core
open! Async.Std

val create : working_dir:string -> files:string list -> string -> unit Deferred.Or_error.t
val extract : working_dir:string -> string -> unit Deferred.Or_error.t
val list : string -> string list Deferred.Or_error.t

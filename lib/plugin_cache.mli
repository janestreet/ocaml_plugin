(**
   This cache avoid recompilation of sources if there are no changes in the filenames.
   Since we want the side effects to be executed in case of a re-loading, this isn't a
   cache of dynloading.
   The purpose of this cache is to speed up the initialization of programs relying of
   an ml config set up. The cache is meant to be persistent between different executions
   of the program. Basically, cmxs files are stored in a specific location.
   This is not a ram cache.
   This module is resistant to version upgrade. The info file contains the version of
   the executable using the cache. If the version doesn't match, the cache is deleted
   (or just skiped if no write access).
*)

open Core.Std
open Async.Std

(**
   Mutable type containing informations about the cache files.
*)
type t

type filename = string

module Digest : sig
  type t
  val to_string : t -> string
end

module Sources : sig
  type t
  val files : t -> (filename * Digest.t) list
  val key : t -> filename list
end

module Plugin : sig
  type t
  val cmxs_filename : t -> string
  val sources : t -> Sources.t
  val plugin_uuid : t -> Plugin_uuid.t
end

module Config : sig
  type t with sexp
  val create :
    dir:string
    -> ?max_files:int (* default is 10 *)
    -> ?readonly:bool (* default is false *)
    -> ?try_old_cache_with_new_exec:bool (* default is false *)
    -> unit
    -> t

  val dir : t -> string
  val max_files : t -> int
  val readonly : t -> bool
  val try_old_cache_with_new_exec : t -> bool
end

(**
   loading info and cache initialization, including some clean-up if needed, etc.
   cleaning old version files if present.
*)
val create : Config.t -> t Deferred.Or_error.t

val digest : Ml_bundle.t list -> Sources.t Deferred.Or_error.t

val find : t -> Sources.t -> Plugin.t option

(** update the info in the file system, perform some clean-up if needed *)
val add : t -> Sources.t -> Plugin_uuid.t -> filename -> unit Deferred.Or_error.t

val old_cache_with_new_exec : t -> bool

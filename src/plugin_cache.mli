(** This cache avoid recompilation of sources if there are no changes in the files.  Since
    we want the side effects to be executed in case of a re-loading, this isn't a cache of
    dynloading.  The purpose of this cache is to speed up the initialization of programs
    relying on an ml config set up. The cache is meant to be persistent between different
    executions of the program.  Basically, cmxs files are stored in a specific location.
    This is not a ram cache.  This module handles version upgrades.  The info file
    contains the version of the executable using the cache.  If the version doesn't match,
    the cache is deleted (or just skipped if no write access). *)

open! Core
open! Async

(** Mutable type containing information about the cached files. *)
type t

type filename = string

module Sources : sig
  type t
end

module Plugin : sig
  type t

  val cmxs_filename : t -> string
  val sources : t -> Sources.t
  val was_compiled_by_current_exec : t -> bool
end

module Config : sig
  type t [@@deriving sexp]

  val create
    :  dir:string
    -> ?max_files:int (* default is 10 *)
    -> ?readonly:bool (* default is false *)
    -> ?try_old_cache_with_new_exec:bool (* default is false *)
    -> unit
    -> t

  val dir : t -> string
  val max_files : t -> int
  val readonly : t -> bool
  val try_old_cache_with_new_exec : t -> bool

  module Stable : sig
    module V1 : Stable_without_comparator

    module V2 : sig
      (** Note that the sexp representation for this [t] is different than for the main
          type, even though they are equal types: the main type's sexp converter includes
          a version tag to try to upgrade more seamlessly. *)
      type nonrec t = t

      include Stable_without_comparator with type t := t

      val of_prev : V1.t -> t
    end
  end
end

(** Loading info and cache initialization, including some clean-up if needed, etc.
    cleaning old version files if present. *)
val create : Config.t -> t Deferred.Or_error.t

val digest : Ml_bundle.t list -> Sources.t Deferred.Or_error.t
val find : t -> Sources.t -> Plugin.t option

(** Update the info in the file system, perform some clean-up if needed. *)
val add : t -> Sources.t -> Plugin_uuid.t -> filename -> unit Deferred.Or_error.t

(** Release this plugin cache lock. *)
val clean : t -> unit Deferred.Or_error.t

(** Exported to be used in some other part of ocaml_plugin. *)
module Digest : sig
  type t [@@deriving compare, sexp]

  include Stringable with type t := t

  val file : filename -> t Deferred.Or_error.t
  val string : string -> t
end

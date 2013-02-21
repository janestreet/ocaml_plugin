(**
   This module offers the minimal api to embed and then retrieve some files
   into an executable. This 'packaging' is done using external async calls to
   [objcopy] from linux binutils package.
*)
open Core.Std
open Async.Std

type filename = string

(**
   Used by [bin/ocaml_embed_compiler.exe]
*)
val embed :
  filename:filename
  -> section_id:string
  -> section:filename
  -> destination:filename
  -> unit Deferred.Or_error.t

val retrieve :
  filename:filename
  -> section_id:string
  -> destination:filename
  -> unit Deferred.Or_error.t

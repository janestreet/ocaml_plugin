open Core.Std
open Async.Std

(**
   Ocaml plugin loader, from ocaml source file.
   Wrapper around Dynlink module, including on the fly async ocaml compilation.

   This is a low level module, casual user should rather use 'Ocaml_compiler'
   apart from Make to create a dedicated Plugin loader.
*)

module Config : sig
  type t = {
    pa_files : string list ;
  } with sexp
end

(**
   mutable type for loading ocaml modules.
*)
type t
type dynloader = t

(**
   Currently this library works with native code only. Called in bytecode,
   the function [create] will return the following exception as an error result.
*)
exception Is_not_native with sexp

(**
   The convention over the name of the executable inside the archive.
   All are native executables (.opt)
*)
val ocamlopt_opt : string
val camlp4o_opt  : string
val ocamldep_opt : string

type 'a create_arguments =
  ?in_dir:string
  (**
     The pathname has to be absolute. If not, the behavior is unspecified.

     This is the location where the ocaml compilation will take place. A fresh directory
     will be created inside that directory. The default value is [Filename.temp_dir_name],
     so the default behavior is to create a fresh directory in the temp_dir_name, like
     /tmp/ocaml_plugin.1464h45 .

     Plugins are copied into that directory, and compiled there. Cf function [clean].

     This should preferably be a local disk location not using NFS networking
     (compilation will be faster).
  *)

  -> ?include_directories:string list
  (**
     If you do not use the Auto_embed mode, you want to use some cmi files
     installed in some shared places. You need then to add these directories
     to the ocamlopt compilation, using -I option. Each directory of this
     list will be added in the command, this module will add the -I flag.

     These should rather be absolute directories. If not, they would be
     concatenated with the cwd at the time the function [create] is executed.
  *)

  -> ?custom_warnings_spec:string
  (**
     When one use this library, warnings are always triggered as errors. This flag allows
     one to change the warning activated and is passed to the compiler using the flag [-w]
     followed by the string provided :
     [-w $(custom_warnings_spec) -warn-error +a]

     By default, the common warnings are activated: "@a-4-29"
     If you want to specify the warning specification yourself, use this flag.
     The format of the string is documented by ocamlopt -help. e.g. "+a-4-7-9-29-28"
  *)

  -> ?strict_sequence:bool
  (** use or don't use -strict-sequence during compilation. Set to [true] by default, so
      that it is consistent with the policy used in core, async, etc. *)

  -> ?cmx_flags:string list
  (**
     Add some flags to the compilation producing the cmx file.
     No check are done, the option are passed 'as they are' to ocamlopt
  *)

  -> ?cmxs_flags:string list
  (**
     Same thing for cmxs compilation
  *)

  -> ?trigger_unused_value_warnings_despite_mli:bool
  (**
     When the files of a plugin export values in some mli but no one is using them,
     unused value warnings are not triggered.
     However, by explicitly setting this parameter to [true] they will be triggered. This
     might come in handy to detect dead code in a large plugin that uses multiple
     files.
  *)

  -> ?use_cache:Plugin_cache.Config.t
  (**
     By default, there is no cache. If a config is given, cmxs files
     may be used from an execution to an other.
  *)

  -> ?run_plugin_toplevel: [ `In_async_thread | `Outside_of_async ]
  (**
     If a plugin has some blocking calls at toplevel that are synchronous, one may want to
     run the plugin's toplevel execution under an [In_thread.run] to make sure it is not
     going to block the main thread. To do it, set this parameter to [`Outside_of_async].
     On the other hand, doing so will prevent the user from having Async functions calls
     at toplevel, unless [Thread_safe] is used carefully.
     The default is [`In_async_thread], meaning this library tends to be more friendly
     with async plugins, as it is probably the most common use for it.
  *)

  -> 'a

val create : (
  ?initialize_compilation_callback:(
    directory:string -> Config.t option Deferred.Or_error.t
  )
  (**
     In case we do not use the cache of cmxs, and the compilation will actually
     takes place, we offer a way via this call back to perform some compuation
     before the first compilation. This is typically when [ocaml_compiler]
     will extract its tar file and load the config embedded in the archive.
  *)

  -> ?ocamlopt_opt:string
  -> ?camlp4o_opt:string
  -> ?ocamldep_opt:string
  (**
     ocamlopt, camlp4 and ocamldep should be for the same version of ocaml as the current
     executable and the provided or embedded ocaml files (interfaces, preprocessors).
     If this is not specified, ocaml_plugin will assume that the correct ocamlopt.opt,
     camlp4.opt or ocamldep.opt' are present in the path of the current executable, which
     is most likely a naive hope.
  *)

  -> ?pa_files:string list
  (**
     A list of syntax extension files required for the plugins' compilation.
     These should be cmxs plugin files and should have been built consistently
     with the given camlp4o_opt, as it will dynlink them.
  *)

  -> unit -> t Deferred.Or_error.t

) create_arguments

(**
   return the lazy_deferred enforcing the initialization of the dynloader.
*)
val compilation_config :
  t
  -> (string * Config.t option) Or_error.t Lazy_deferred.t

(**
   Cleaning the files generated by this Ocaml_dynloader.t from the begining of his life,
   and try to remove the directory if it is empty once the files have been removed.
   Doesn't fail if the directory contains other files, keep them and keep the directory
   in that case. Once cleaned, you cannot use a dynloader anymore, you should just leave
   it alone and let it be collected by the GC at some point. Using a cleaned dynloader
   will result in an error.
*)
val clean : t -> unit Deferred.Or_error.t

module Univ_constr : sig
  type 'a t
  val create : unit -> 'a t
end

module type Module_type =
sig
  (**
     The type [t] is the type of a first level module you want to load.
     This is typically the type of your expected config file, as a top level
     ocaml module.

     The field [repr] is the concrete OCaml syntax for this module type.

     The field [univ_constr] is used to constr and match_ values of type t, embedded
     in a value of type Univ.t.

     The field [univ_constr_repr] is the concrete OCaml syntax for the field [univ_constr].

     Example :
     {module M : A.S} defined in the library "mylib.cmxa".

     {[
     module My_config_loader = Ocaml_plugin.Ocaml_dynloader.Make (
     struct
       type t = (module A.S)
       let repr = "Mylib.A.S"
       let univ_constr = A.univ_constr
       let univ_constr_repr = "Mylib.A.univ_constr"
     end)
     ]}

     [repr] and [univ_constr_repr] should be contain complete paths, as it would be
     used by an ocaml file to link with the shared cmi files, in particular be aware
     that if you have some 'open' statements in your file, you might have different
     t and repr, which is a bad practice.

     If the module type [A.M_intf] is defined in a package, you would need
     to add it in the repr, as it is part of the complete path of the module type
     ("Mylib" in the example).
  *)
  type t
  val t_repr : string
  val univ_constr : t Univ_constr.t
  val univ_constr_repr : string

  (*
    This implementation is type safe. Some properties should be verified so that
    the library would work properly:

    -the type [t] and its representation '[t_repr]' should match,
    -the plugin implementation doesn't override the module type sig
     represented by the string [t_repr]:
    -the plugin implementation doesn't override the univ_constr scope name
     represented by the string [univ_constr_repr]
    -the value [univ_constr] and its representation [univ_constr_repr] should match.
  *)
end

(** [find_dependencies t file] uses ocamldep to compute the list of .ml and .mli files
    that [file] depends on transitively, which you can then pass to [load_ocaml_src_files].
    [file] must be an .ml file, and all the files it depend on must be in the same folder.
*)
val find_dependencies : t -> string -> string list Deferred.Or_error.t

module type S = sig
  type t

  (** Load a bunch of ocaml files source files (.ml + .mli). The last module's signature
      should be compatible with the signature [X.repr]. If the type does not match, there
      will be an error during OCaml compilation. The files are copied into the compilation
      directory, and compiled versus a generated mli file including the relevant module
      signature. This generated file is then dynlinked with the current executable.

      The compilation happens using [Dynlink.loadfile_private], meaning that
      the toplevel definition defined in these files are hidden
      (cannot be referenced) from other modules dynamically loaded afterwards *)
  val load_ocaml_src_files :
    dynloader -> string list -> t Deferred.Or_error.t

  (** Similar to [load_ocaml_src_files], but does not execute the plugin toplevel, just
      checks that compilation and dynamic linking work. *)
  val check_ocaml_src_files :
    dynloader -> string list -> unit Deferred.Or_error.t

  (** The following functions are used for step by step use, not for the casual user
      because they are much more error prone. Prefer [load_ocaml_src_files] if possible *)

  (** This compiles the source files into cmxs file, but does not execute the plugin
      toplevel. The resulting cmxs file can be loaded by [blocking_load_cmxs_file] either
      from the same process or other processes which share the same executable. If compile
      succeeds, it returns [Ok] and write the compiled cmxs file into [output_file] (may
      override existing file), otherwise it returns [Error] and won't write to
      [output_file] at all. *)
  val compile_ocaml_src_files_into_cmxs_file
    :  dynloader
    -> string list
    -> output_file:string (* like -o option of gcc *)
    -> unit Deferred.Or_error.t

  (** Dynlink has the following not really wanted property: dynlinking a file with a given
      filename only works properly the first time. Further dynlinks with the same filename
      (even a different file) will not load the new module but instead execute the initial
      module. Some even says that the behavior upon reload depends on the platform. Long
      story short: don't do that. Dynlink files at most once.

      It is worth noting too that this function only works with cmxs files produced by
      ocaml_plugin's [compile_ocaml_src_files_into_cmxs_file]. It expects the code to
      perform some internal library calls, thus it cannot be used with any arbitrary cmxs
      compiled in some other way. Furthermore this function would return an error even
      though the cmxs was built with ocaml_plugin when built under a different context
      (compiler version used, cmi dependencies version, etc.)  The intended usage is to
      have the compilation and loading done using the same executable. *)
  val blocking_load_cmxs_file : string -> t Or_error.t
end

module Make : functor (X : Module_type) -> S with type t := X.t

(** In some cases, we are only interested by the toplevel side effects of dynlinked
    modules. *)
module Side_effect : S with type t := unit

(* =============================================================== *)
(* The following section is for internal use only*)
module type Side_effect = sig
end
val side_effect_univ_constr : (module Side_effect) Univ_constr.t
val return_plugin : 'a Univ_constr.t -> (unit -> 'a) -> unit
(* =============================================================== *)


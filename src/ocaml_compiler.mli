(** This module offers an higher level api over Ocaml_dynloader.  This retrieves the
    compiler and a bunch of cmi files embedded in the current executable, and use them to
    do the compilation of ocaml source files, before loading them using Dynlink. The
    compilation steps happen in Async.

    This is meant to be used in unix only, in particular because it uses /proc to
    determine the location of the exec code being run. *)
open! Core.Std
open! Async.Std

(**
   The convention over the name of the executables inside the archive.
   All are native executables (.opt)
*)
val ocamlopt_opt : string
val camlp4o_opt  : string
val ocamldep_opt : string
val ppx_exe      : string

(** Mutable type to get the compiler and the cmi which must have been embedded in the
    executable. *)
type t

module Archive_metadata : sig
  type t =
    { camlp4_is_embedded : [`pa_files of string list] option
    ; ppx_is_embedded    : bool
    ; archive_digests    : Plugin_cache.Digest.t String.Map.t
    }
  [@@deriving sexp_of]
end

module Code_style : sig
  type t =
    [ `No_preprocessing
    | `Camlp4_style
    | `Ppx_style
    ]
  [@@deriving sexp_of]

  val arg_type       : t Command.Arg_type.t
  val optional_param : t option Command.Spec.param
end

type 'a create_arguments = (
  ?code_style:Code_style.t
  (** Code_style can be specified statically in a jbuild...

      {v
       (embed (
         ...
         (code_style ppx)))
      v}

      ...or dynamically using the above [code_style] create_argument.

      The static selection controls which preprocessors are embedded in the archive.  The
      dynamic controls which preprocessor (if any) to use when loading a plugin.

      The dynamically requested preprocessor shall be present in the archive, otherwise
      an exception will be raised during the compilation of the plugin.

      If the optional [code_style] argument is not given, the behaviour is determined by
      the contents of the archive:

      - if neither preprocessor is embedded, do [No_preprocessing].
      - if just one preprocessor (camlp4 or ppx) is embedded, it is used.
      - if both are embedded, use ppx (it's the future!).

      A simple migration path which requires no application code changes is:
      - select (code_style ppx) in the jbuild.
      - build & roll out a new executable, and switch all plugins to ppx-style.
      (The new executable will contain only the ppx preprocessor)

      A more complex migration path (requires application code changes):
      - select (code_style bilingual)
      - Modify the plugin-app to select dynamically the [code_style] to use;
      perhaps using a command line flag, or other config parameters.
      See [ ../test/plugin_loader.ml].
      - Build and roll out; Adapt plugins to ppx-style as required.
      (The new executable will contain both camlp4 & ppx preprocessors) *)

  -> ?persistent_archive_dirpath:string
  (** Keep the extracted archive in some persistent location to avoid paying the cost of
      extraction each time a file needs to be compiled.  The location passed will not be
      cleaned at the end of the execution of the program. The only guarantee given there
      is that it is never going to grow more than the size of the embedded archive.  If
      the persistent location contains a extracted version that is older than the current
      executable, the directory is cleaned up and the archive is extracted again. *)

  -> 'a
) Ocaml_dynloader.create_arguments

(** This is a special utilisation of the Generic Loader.  It relies on a few assumptions,
    such as a file called ocamlopt.opt is present in the archive, as well as some cmi
    files, and uses a tmp compilation_directory.  A call to this function will retrieve
    the embedded part of the code, and extract it in the current corresponding directory.
    The most common use and the default value for [code_file] is [`my_code].  Currently
    this library works with native code only. Called in bytecode, this function will
    raise.  See the documentation of ocaml_dynloader for other flags, they are passed
    internally to that module to create the internal dynloader (in _dir,
    custom_warnings_spec, etc.).

    /!\ By using this manual create, you take the responsibility to call [clean t] when
    you're done with all the compilation that you want to do with this compiler.  Consider
    using [with_compiler], [Make] or [load_ocaml_src_files] if this makes your life
    simpler. *)
val create : (
  unit -> [`this_needs_manual_cleaning_after of t] Deferred.Or_error.t
) create_arguments

(** Call create, do something with the compiler, and then take care of calling clean.  In
    case an exception or a shutdown happen and f never returns, an attempt to clean the
    compiler is still done via an at_shutdown execution. *)
val with_compiler : (
  f:(t -> 'a Deferred.Or_error.t)
  -> unit -> 'a Deferred.Or_error.t
) create_arguments

(** Get the loader using this compiler and these cmi. *)
val loader : t -> Ocaml_dynloader.t

(** This will delete the temporary directory created, and remove all the files, included
    the files generated by the loader.  This function should be used when the compiler has
    been created using [create] for advanced use of this module. For a simpler usage, look
    at [with_compiler] or the functor below. *)
val clean : t -> unit Deferred.Or_error.t

module type S = sig
  type t

  val load_ocaml_src_files : (
    string list -> t Deferred.Or_error.t
  ) create_arguments

  val load_ocaml_src_files_without_running_them : (
    string list -> (unit -> t) Deferred.Or_error.t
  ) create_arguments

  val check_ocaml_src_files : (
    string list -> unit Deferred.Or_error.t
  ) create_arguments

  (** Command that checks that the anon files given compile and match the interface [X]
      given. Also provides a [-ocamldep] mode allowing only the main file to be passed on
      the command line.

      When [with_code_style_switch:false], the code style is the same as for
      [with_compiler].  One may use [true] if both camlp4 and ppx are embedded, in which
      case a [-code-style] flag is provided in the command line to pick between camlp4 and
      ppx to perform the validation. *)
  val check_plugin_cmd :
    with_code_style_switch:bool
    -> unit
    -> Command.t

  (** [Load] contains functions similar to the ones above, and can be used if you want to
      separate extraction of the compiler from building/loading files. It can be useful if
      you want to extract the compiler upfront (to improve latency a bit) or if you want
      to share the cost of extracting the compiler for the current executable over
      multiple compilations. Probably not needed by the casual user. *)
  module Load : Ocaml_dynloader.S with type t := t
end

(** This is a wrapper for the similar module in Ocaml_dynloader that takes care of
    cleaning the compiler afterwards. *)
module Make : functor (X : Ocaml_dynloader.Module_type) -> S with type t := X.t

(** In some cases, we are not interested by the module, but rather because it uses a side
    effect registering mechanism. *)
module Side_effect : S with type t := unit

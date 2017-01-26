(* In the public release, this module is replaced by:

   {[
     let build_info_as_sexp =
       Sexplib.Sexp.Atom (Digest.to_hex (Digest.file Sys.executable_name))

     let version = "NO_VERSION_UTIL"
   ]}
*)
include Core.Version_util

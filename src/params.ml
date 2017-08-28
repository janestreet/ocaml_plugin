(* In the public release, this module is replaced by:

   {[
     let build_info_as_sexp =
       Sexplib.Sexp.Atom (Md5.to_hex (Md5.digest_file_blocking Sys.executable_name))

     let version = "NO_VERSION_UTIL"
   ]}
*)
include Core.Version_util

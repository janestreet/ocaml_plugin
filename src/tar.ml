open! Core
open! Async

let extract ~working_dir tar =
  let args = [
    "-xzf"; tar;
  ] in
  Shell.run ~working_dir "tar" args
;;

let list tar =
  (* curiously, tar doesn't need a z option *)
  let args = [ "-tf" ; tar ] in
  Shell.run_lines "tar" args
;;

let create ~working_dir ~files tar =
  (* The tar should be deterministic if possible, to avoid causing spurious rebuilds.
     This option isn't supported on mac os, it seems, so we use this conditionally. *)
  let deterministic_archive_flag = [ "--mtime"; "2017-01-01" ] in
  let deterministic_env = [ ("GZIP", "-n") ] in
  Process.run ~working_dir ~prog:"tar"
    ~args:(deterministic_archive_flag @ [ "--version" ]) ()
  >>| Result.is_ok
  >>= fun support_mtime ->
  let args =
    (if support_mtime then deterministic_archive_flag else [])
    @ [ "-czf"; tar ]
    @ files
  in
  Shell.run ~env:(`Extend deterministic_env) ~working_dir "tar" args
;;

open Core.Std
open Async.Std

let permission_exe = 0o700
;;

let echo    = ref false
let verbose = ref false
;;

let set_defaults ?verbose:(v = !verbose) ?echo:(e = !echo) () =
  echo := e;
  verbose := v;
  ()
;;

module Process_flag =
struct
  let echo =
    Command.Spec.(
      flag
        "-shell-echo"
        no_arg
        ~doc:" show external shell calls"
    )
  ;;

  let verbose =
    Command.Spec.(
      flag
        "-shell-verbose"
        no_arg
        ~doc:" let external shell call be more verbose (imply -shell-echo)"
    )
  ;;

  let all () = Command.Spec.(step (fun main echo verbose ->
    set_defaults ~echo ~verbose ();
    main
  ) +> echo +> verbose)
  ;;
end

let flags = Process_flag.all
;;

let endline std = if std = "" then std else std ^ "\n"
;;

let make_run from_output ?working_dir ?(quiet_or_error = false) prog args =
  let command_text =
    lazy (prog::args |> [%sexp_of: string list] |> Sexp.to_string)
  in
  if !echo then
    Core.Std.Printf.printf "Shell: %s\n%!" (force command_text);

  Process.create ?working_dir ~prog ~args () >>=? fun process ->
  Process.collect_output_and_wait process >>| fun output ->
  let { Process.Output.stdout ; stderr ; exit_status } = output in

  let error_and_status =
    match exit_status with
    | Error status -> Error (Some status)
    | Ok () ->
      if quiet_or_error && (stdout <> "" || stderr <> "")
      then Error None
      else Ok ()
  in
  if !verbose then (
    Core.Std.Printf.printf "%s%s%!" (endline stdout) (endline stderr);
  );
  match error_and_status with
  | Ok () ->
    Ok (from_output output)
  | Error status ->
    let working_dir =
      match working_dir with
      | Some working_dir -> working_dir
      | None -> "none (cwd)"
    in
    let error = Error.of_lazy (lazy (
      (* not using an sexp_of_t because it makes the output unreadable by escaping
         newlines *)
      let status =
        match status with
        | Some status -> Sexp.to_string
                           ([%sexp_of: Core.Std.Unix.Exit_or_signal.error] status)
        | None -> "error trace on stdout or stderr"
      in
      sprintf "working_dir: %s\nstatus: %s\ncommand: %s\n%s%s"
        working_dir
        status
        (force command_text)
        stdout
        (if stdout = "" then stderr else "\n"^stderr)
    ))
    in
    Error error
;;

let run = make_run ignore
;;

let run_lines =
  make_run ~quiet_or_error:false
    (function { Process.Output.stdout ; _ } ->
       List.filter_map (String.split ~on:'\n' stdout)
         ~f:(fun s -> let s = String.rstrip s in if s = "" then None else Some s))
;;

let getcwd () =
  Deferred.Or_error.try_with ~name:"Ocaml_plugin.Shell.getcwd" Sys.getcwd
;;

let chmod pathname ~perm =
  Deferred.Or_error.try_with ~extract_exn:true (fun () -> Unix.chmod pathname ~perm)
;;

let raw_temp_dir ~in_dir ?(prefix="ocaml_plugin_") ?(suffix=".build")
      ?(perm=permission_exe) () =
  let fct () =
    Filename.temp_dir
      ~perm
      ~in_dir
      prefix
      suffix
  in
  Deferred.Or_error.try_with ~extract_exn:true (fun () ->
    Unix.mkdir ~p:() ~perm in_dir >>= fun () ->
    In_thread.run fct)
;;

let absolute_pathname filename =
  if Filename.is_relative filename
  then
    getcwd () >>|? fun prefix ->
    prefix ^/ filename
  else
    Deferred.return (Ok filename)
;;

let absolute_pathnames filenames =
  let relative = ref false in
  let map filename =
    if Filename.is_relative filename
    then (relative := true; `relative filename)
    else `absolute filename
  in
  let filenames = List.rev_map ~f:map filenames in
  if !relative
  then (
    getcwd () >>|? fun cwd ->
    let files = List.rev_map filenames ~f:(function
      | `absolute filename -> filename
      | `relative filename -> cwd ^/ filename
    ) in
    files
  )
  else
    let files = List.rev_map filenames ~f:(function
      | `absolute filename -> filename
      | `relative _ -> assert false
    ) in
    Deferred.return (Ok files)
;;

(* this should return an absolute pathname *)
let temp_dir ~in_dir ?prefix ?suffix ?perm () =
  raw_temp_dir ~in_dir ?prefix ?suffix ?perm () >>=? absolute_pathname
;;

let rm ?r ?f paths =
  let r = Option.map r ~f:(fun () -> "-r") in
  let f = Option.map f ~f:(fun () -> "-f") in
  run "/bin/rm" (List.filter_map ~f:ident [r; f] @ ("--" :: paths))
;;

let rmdir dir =
  run "/bin/rmdir" [ dir ]
;;

let cp ~source ~dest =
  run "/bin/cp" [ source ; dest ]
;;

let readdir dir =
  Deferred.Or_error.try_with (fun () ->
    Sys.readdir dir
  )
;;

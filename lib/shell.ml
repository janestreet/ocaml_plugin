open Core.Std
open Async.Std

module Deferred = struct
  let return = Deferred.return
  module Or_error = struct
    type 'a t = 'a Deferred.Or_error.t
    let try_with ?(extract_exn=false) ?name f =
      Deferred.map (Monitor.try_with ?name f) ~f:(function
      | Error exn ->
        let exn = if extract_exn then Monitor.extract_exn exn else exn in
        Error (Error.of_exn exn)
      | Ok _ as ok -> ok)

    let try_with_join ?extract_exn ?name f =
      Deferred.map (try_with ?extract_exn ?name f) ~f:Or_error.join
  end
end

let permission_exe = 0o700

let echo = ref false
let verbose = ref false

let set_defaults ?verbose:(v = !verbose) ?echo:(e = !echo) () =
  echo := e;
  verbose := v;
  ()

module Process_flag =
struct
  let echo =
    Command.Spec.(
      flag
        "-shell-echo"
        no_arg
        ~doc:" show external shell calls"
    )

  let verbose =
    Command.Spec.(
      flag
        "-shell-verbose"
        no_arg
        ~doc:" let external shell call be more verbose (imply -shell-echo)"
    )

  let all () = Command.Spec.(step (fun main echo verbose ->
    set_defaults ~echo ~verbose ();
    main
  ) +> echo +> verbose)
end

let flags = Process_flag.all

let endline std = if std = "" then std else std ^ "\n"

let make_run from_output ?working_dir ?(quiet_or_error = false) cmd args =
  if !echo then
    Printf.printf "Shell: %s %s\n%!" cmd (String.concat ~sep:" " args);

  Process.create ?working_dir ~prog:cmd ~args () >>=? fun process ->
    Process.wait process >>| fun output ->
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
      Printf.printf "%s%s%!" (endline stdout) (endline stderr);
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
            (<:sexp_of< Core.Std.Unix.Exit_or_signal.error >> status)
          | None -> "error trace on stdout or stderr"
        in
        sprintf "working_dir: %s\nstatus: %s\ncommand: %s %s\n%s%s"
          working_dir
          status
          cmd (String.concat ~sep:" " args)
          stdout
          (if stdout = "" then stderr else "\n"^stderr)
      ))
      in
      Error error

let run = make_run ignore
let run_lines = make_run ~quiet_or_error:false
  (function { Process.Output.stdout ; _ } ->
    let list = String.split ~on:'\n' stdout in
    let list = List.filter_map list
      ~f:(fun s -> let s = String.rstrip s in if s = "" then None else Some s)
    in
    list
  )

let mkdir_p ?(perm=permission_exe) path =
  run "/bin/mkdir" [ "-p" ; sprintf "--mode=%o" perm ; "--" ; path ]

let getcwd () =
  Deferred.Or_error.try_with ~name:"Ocaml_plugin.Shell.getcwd" Sys.getcwd

let chmod pathname ~perm =
  Deferred.Or_error.try_with ~extract_exn:true (fun () -> Unix.chmod pathname ~perm)

let raw_temp_dir ~in_dir =
  let fct () =
    Filename.temp_dir
      ~perm:permission_exe
      ~in_dir
      "ocaml_plugin_"
      ".build"
  in
  mkdir_p ~perm:permission_exe in_dir >>=? fun () ->
  Deferred.Or_error.try_with ~extract_exn:true (fun () -> In_thread.run fct)

let absolute_pathname filename =
  if Filename.is_relative filename
  then
    getcwd () >>|? fun prefix ->
      prefix ^/ filename
  else
    Deferred.return (Ok filename)

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

(* this should return an absolute pathname *)
let temp_dir ~in_dir =
  raw_temp_dir ~in_dir >>=? absolute_pathname

let rm ?r ?f paths =
  let r = Option.map r ~f:(fun () -> "-r") in
  let f = Option.map f ~f:(fun () -> "-f") in
  run "/bin/rm" (List.filter_map ~f:ident [r; f] @ ("--" :: paths))

let rmdir dir =
  run "/bin/rmdir" [ dir ]

let cp ~source ~dest =
  run "/bin/cp" [ source ; dest ]

let readdir dir =
  Deferred.Or_error.try_with (fun () ->
    Sys.readdir dir
  )

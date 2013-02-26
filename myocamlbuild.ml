(* OASIS_START *)
(* OASIS_STOP *)
# 4 "myocamlbuild.ml"

let protectx x ~f ~finally =
  let r = try f x with exn -> finally x; raise exn in
  finally x; r

let rm_rf dir =
  ignore (Printf.ksprintf Sys.command "/bin/rm -rf %S" dir : int)

let temp_dir ?(in_dir = Filename.temp_dir_name) prefix suffix =
  let base = Filename.concat in_dir prefix in
  let rec loop i =
     let dir = base ^ string_of_int i ^ suffix in
     let ret = Printf.ksprintf Sys.command "/bin/mkdir %S 2>/dev/null" dir in
     if ret = 0 then dir
     else if Sys.file_exists dir then loop (i + 1)
     else failwith ("mkdir failed on " ^ dir)
   in loop 0

let read_lines ic =
  let rec loop acc =
    match try Some (input_line ic) with End_of_file -> None with
    | Some line -> loop (line :: acc)
    | None -> List.rev acc
  in loop []

let test cmd =
  match Sys.command cmd with
  | 0 -> true
  | 1 -> false
  | _ -> failwith ("command ^cmd^ failed.")

let sh_lines cmd =
  protectx (Filename.temp_file "ocamlbuild_cmd" ".txt")
    ~f:(fun fn ->
      ignore (Sys.command ("(" ^ cmd ^ ") >" ^ fn) : int);
      protectx (open_in fn) ~f:read_lines ~finally:close_in)
    ~finally:Sys.remove

let getconf var =
  let cmd = Printf.sprintf "getconf %S" var in
  match sh_lines cmd with
  | []  -> None
  | [x] -> Some x
  | _   -> failwith ("`"^cmd^"` returned multiple lines")

let endswith x s =
  let len_x = String.length x and len_s = String.length s in
  (len_x <= len_s) && x = String.sub s (len_s - len_x) len_x

let select_files dir ext =
  List.map (Filename.concat dir)
    (List.filter (endswith ext)
      (Array.to_list (Sys.readdir dir)))
;;


let setup_standard_build_flags () =
    begin match getconf "LFS64_CFLAGS" with
    | None -> ()
    | Some flags -> flag ["compile"; "c"] (S[A"-ccopt"; A flags])
    end;
    let cflags =
      let flags =
        [
          "-pipe";
          "-g";
          "-fPIC";
          "-O2";
          "-fomit-frame-pointer";
          "-fsigned-char";
          "-Wall";
          "-pedantic";
          "-Wextra";
          "-Wunused";
(*          "-Werror"; *)
          "-Wno-long-long";
        ]
      in
      let f flag = [A "-ccopt"; A flag] in
      List.concat (List.map f flags)
    in
    flag ["compile"; "c"] (S cflags);

    (* enable warnings; make sure the '@' character isn't in the beginning;
       ms-dos interprets that character specially *)
    flag ["compile"; "ocaml"] (S [A "-w"; A "Aemr-28"; A "-strict-sequence" ])
;;

let dispatch hook =
  dispatch_default hook;
  match hook with
  | Before_options ->
    Options.make_links := false

  | After_rules ->
    setup_standard_build_flags ();

    let env = BaseEnvLight.load () in
    let stdlib = BaseEnvLight.var_get "standard_library" env in
    rule "standalone"
      ~deps:["%.native"; "bin/ocaml_embed_compiler.native"]
      ~prod:"%_standalone.native"
      (fun env build ->
        let ocaml_embed_compiler = "bin/ocaml_embed_compiler.native" in
        let ocamlopt = Command.search_in_path "ocamlopt.opt" in
        let camlp4o = Command.search_in_path "camlp4o.opt" in

        (* Build the list of explicit dependencies. *)
        let packages =
          Tags.fold
            (fun tag packages ->
              if String.is_prefix "pkg_" tag then
                let idx = try String.index tag '.' with Not_found -> String.length tag in
                StringSet.add (String.sub tag 4 (idx - 4)) packages
              else
                packages)
            (tags_of_pathname (env "%_standalone.native"))
            StringSet.empty
        in

        (* Build the list of dependencies. *)
        let deps =
          Findlib.topological_closure
            (List.map Findlib.query (StringSet.elements packages))
        in
        (* Build the set of locations of dependencies. *)
        let locs =
          List.fold_left
            (fun set pkg -> StringSet.add pkg.Findlib.location set)
            StringSet.empty deps
        in
        (* Directories to search for .cmi and .cmxs (for camlp4o): *)
        let directories =
          List.fold_left
            (fun acc dir -> StringSet.add dir acc)
            locs
            ["lib";
             Pathname.dirname (env "%");
             stdlib;
             stdlib / "threads"]
        in
        (* List of .cmi and .cmxs (for camlp4o.opt): *)
        let cmi_set, cmxs_set =
          StringSet.fold
            (fun directory acc ->
              List.fold_left
                (fun (cmi_set, cmxs_set) fname ->
                  if Pathname.check_extension fname "cmi" then
                    (StringSet.add (directory / fname) cmi_set, cmxs_set)
                  else if Pathname.check_extension fname "cmxs"
                      && String.is_prefix "pa_" fname then
                    (cmi_set, StringSet.add (directory / fname) cmxs_set)
                  else
                    (cmi_set, cmxs_set))
                acc
                (Array.to_list (Pathname.readdir directory)))
            directories (StringSet.empty, StringSet.empty)
        in
        let camlp4 =
          if StringSet.is_empty cmxs_set then
            S []
          else
            S [A "-pp"; P camlp4o;
               S (List.map
                    (fun cmxs -> S [A "-pa-cmxs"; P cmxs])
                    (StringSet.elements cmxs_set))]
        in
        Cmd (S [P ocaml_embed_compiler;
                A "-exe"; A (env "%.native");
                camlp4;
                A "-cc"; A ocamlopt;
                S (List.map (fun cmi -> A cmi) (StringSet.elements cmi_set));
                A "-o"; A (env "%_standalone.native")]))

  | _ ->
    ()

let () = Ocamlbuild_plugin.dispatch dispatch

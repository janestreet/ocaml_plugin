(* OASIS_START *)
(* OASIS_STOP *)
# 4 "myocamlbuild.ml"

let dispatch = function
  | Before_options ->
    Options.make_links := false

  | After_rules ->
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

let () = Ocamlbuild_plugin.dispatch (fun hook -> dispatch hook; dispatch_default hook)

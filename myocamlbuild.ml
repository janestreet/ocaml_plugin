(* OASIS_START *)
(* OASIS_STOP *)
# 4 "myocamlbuild.ml"

module JS = Jane_street_ocamlbuild_goodies

let dev_mode = true

let dispatch = function
  | Before_options ->
    Options.make_links := false

  | After_rules ->
    let env = BaseEnvLight.load () in
    let stdlib = BaseEnvLight.var_get "standard_library" env in
    rule "ocaml_plugin standalone archive"
      ~deps:["src/ocaml_plugin.cmi";
             "sample/dsl.cmi";
             "hello_world/plugin_intf.cmi";
             "bin/ocaml_embed_compiler.native"]
      ~prod:"bin/ocaml_sample_archive.c"
      (fun _ _ ->
        let loc pkg = (Findlib.query pkg).Findlib.location in
        Cmd (S [P "bin/ocaml_embed_compiler.native";
                A "-pp"; P (Command.search_in_path "camlp4o.opt");
                A "-pa-cmxs"; P (loc "type_conv" / "pa_type_conv.cmxs");
                A "-pa-cmxs"; P (loc "sexplib" / "pa_sexp_conv.cmxs");
                A "-pa-cmxs"; P (loc "fieldslib" / "pa_fields_conv.cmxs");
                A "-cc"; A (Command.search_in_path "ocamlopt.opt");
                A (stdlib / "pervasives.cmi");
                A (loc "core" / "core.cmi");
                A (loc "fieldslib" / "fieldslib.cmi");
                A (loc "sexplib" / "sexplib.cmi");
                A "src/ocaml_plugin.cmi";
                A "sample/dsl.cmi";
                A "hello_world/plugin_intf.cmi";
                A "-o"; A "bin/ocaml_sample_archive.c"]))

  | _ ->
    ()

let () =
  Ocamlbuild_plugin.dispatch (fun hook ->
    JS.alt_cmxs_of_cmxa_rule hook;
    JS.pass_predicates_to_ocamldep hook;
    if dev_mode && not Sys.win32 then JS.track_external_deps hook;
    Ppx_driver_ocamlbuild.dispatch hook;
    dispatch hook;
    dispatch_default hook)

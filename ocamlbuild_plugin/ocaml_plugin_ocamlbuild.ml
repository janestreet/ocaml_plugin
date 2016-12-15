open Ocamlbuild_plugin

(* This list is copied from the jenga/root.ml file used at Jane Street. It should be
   enough for most cases. *)
let stdlib_cmi_to_embed =
  [ "pervasives.cmi"
  ; "camlinternalLazy.cmi"
  ; "camlinternalMod.cmi"
  ; "camlinternalOO.cmi"
  ; "camlinternalFormatBasics.cmi"
  ; "lexing.cmi"
  ; "printf.cmi"
  ; "digest.cmi"
  ]

let embed ~program ~libraries ?(local_cmi_files=[]) ?ppx () =
  let libraries =
    if List.mem "ocaml_plugin" libraries then
      libraries
    else
      "ocaml_plugin" :: libraries
  in
  let target = program ^ ".archive.c" in
  let tag = "file:" ^ program in
  dep [tag] [program ^ ".archive.o"];
  let ppx, deps =
    match ppx with
    | None -> (N, local_cmi_files)
    | Some exe ->
      let exe, deps =
        if String.contains exe '/' then
          (exe, exe :: local_cmi_files)
        else
          (Command.search_in_path exe, local_cmi_files)
      in
      (S [A "-ppx"; A exe], deps)
  in
  rule (Printf.sprintf "embed %s" program)
    ~deps
    ~prod:target
    (fun _env _build ->
       let ocaml_embed_compiler = Command.search_in_path "ocaml-embed-compiler" in
       let ocamlopt = Command.search_in_path "ocamlopt.opt" in
       let ocamldep = Command.search_in_path "ocamldep.opt" in
       let stdlib = String.trim (Printf.ksprintf run_and_read "%s -where" ocamlopt) in
       let cmi_list =
         List.concat
           [ List.map (fun fn -> stdlib / fn) stdlib_cmi_to_embed
           ; local_cmi_files
           ; List.map (fun pkg -> (Findlib.query pkg).Findlib.location / pkg ^ ".cmi")
               libraries
           ]
       in
       Cmd (S [ P ocaml_embed_compiler
              ; A "-cc"; A ocamlopt
              ; A "-ocamldep"; A ocamldep
              ; ppx
              ; Command.atomize cmi_list
              ; A "-o"; A target
              ]))

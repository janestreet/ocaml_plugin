let package_name = "ocaml_plugin"

let sections =
  [ ("lib",
    [ ("built_lib_ocaml_plugin", None)
    ; ("built_lib_ocaml_plugin_ocamlbuild", None)
    ],
    [ ("META", None)
    ])
  ; ("bin",
    [ ("built_exec_ocaml-embed-compiler", Some "ocaml-embed-compiler")
    ],
    [])
  ]

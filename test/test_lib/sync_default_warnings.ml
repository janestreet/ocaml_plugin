open! Core

(*
   {[
     let%expect_test "monitor jenga and ocaml-plugin default warnings" =
       let print_list name set =
         print_endline "--";
         print_endline name;
         Set.iter set ~f:(fun t -> print_endline (Int.to_string t))
       in
       let jenga = Jenga_rules.Compiler_config.disabled_warnings |> Int.Set.of_list in
       let ocaml_plugin = Ocaml_plugin.Dynloader.disabled_warnings |> Int.Set.of_list in
       print_list "jenga" jenga;
       print_list "ocaml-plugin" ocaml_plugin;
       print_list "disabled-only-in-jenga" (Set.diff jenga ocaml_plugin);
       print_list "disabled-only-in-ocaml-plugin" (Set.diff ocaml_plugin jenga);
       [%expect {| |}]
     ;;
   ]}
*)

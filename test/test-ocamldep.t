  $ setup

Loading several ml files with ocamldep in a read-only folder

  $ rm -f *.ml*
  $ mkdir -p readonly
  $ echo 'let () = print_endline "this should not be loaded"' > readonly/test.ml
  $ echo 'let () "this syntax error shall not be a problem"' > readonly/test_with_typo.ml
  $ echo 'open Core;; let x = Test2.x ^ (sprintf !"%{sexp: string}" "-middle")' > readonly/test1.ml
  $ echo 'let x = "first"' > readonly/test2.ml
  $ echo '#!/usr/bin/env ocaml' > readonly/test3.ml
  $ echo 'let x = Test1.x ^ "-last";; let () = print_endline x' >> readonly/test3.ml
  $ chmod -R -w readonly
  $ VERBOSE= $TEST_DIR/plugin_loader.exe --find-dependencies readonly/test3.ml
  Loaded test2.ml test1.ml test3.ml
  first-middle-last
  $ chmod -R +w readonly

Loading several ml files with different cases and using ocamldep

  $ rm -f *.ml*
  $ echo 'let y = "foo"' > Test.ml
  $ echo 'val y : string' > Test.mli
  $ echo '#!/usr/bin/env ocaml' > test1.ml
  $ echo 'let () = print_endline Test.y;; let x = Test.y' >> test1.ml
  $ echo 'val x : string' > test1.mli
  $ VERBOSE= $TEST_DIR/plugin_loader.exe --find-dependencies test1.ml
  Loaded Test.mli Test.ml test1.mli test1.ml
  foo

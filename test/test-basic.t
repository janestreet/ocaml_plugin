  $ setup

Loading non existing mli/ml

  $ rm -f *.ml*
  $ $TEST_DIR/plugin_loader.exe ./test.mli |& matches "test.mli: No such file or directory"
  [1]
  $ $TEST_DIR/plugin_loader.exe ./test.ml |& matches "test.ml: No such file or directory"
  [1]

Loading mli without ml

  $ rm -f *.ml*
  $ touch test.mli
  $ $TEST_DIR/plugin_loader.exe ./test.mli |& matches "test.ml: No such file or directory"
  [1]

Loading ml without mli

  $ rm -f *.ml*
  $ echo 'let () = print_endline "loaded"' > test.ml
  $ $TEST_DIR/plugin_loader.exe ./test.ml
  loaded

Loading ml with mli by mentioning the ml/mli/ml without extension

  $ rm -f *.ml*
  $ touch test.mli
  $ echo 'let () = print_endline "loaded"' > test.ml
  $ $TEST_DIR/plugin_loader.exe ./test.mli
  loaded
  $ $TEST_DIR/plugin_loader.exe ./test.ml
  loaded
  $ $TEST_DIR/plugin_loader.exe ./test
  loaded

Loading ml with mli by mentioning the wrong mli

  $ rm -f *.ml*
  $ touch test.ml
  $ echo 'val a : int' > test.mli
  $ $TEST_DIR/plugin_loader.exe ./test.mli |& matches "The .* \`a' is required"
  [1]

Loading the same file several times

  $ rm -f *.ml*
  $ echo 'let () = print_endline "loaded"' > test.ml
  $ $TEST_DIR/plugin_loader.exe 'test.ml | test.ml | test.ml'
  loaded
  loaded
  loaded

Loading a file that blows up at toplevel

  $ rm -f *.ml*
  $ echo 'let () = failwith "blowup"' > test.ml
  $ $TEST_DIR/plugin_loader.exe ./test.ml
  ("Exception while executing the plugin's toplevel" (Failure blowup))
  [1]

Loading several ml files

  $ rm -f *.ml*
  $ echo '#!/usr/bin/env ocaml' > test1.ml
  $ echo 'let x = Test2.x ^ "-middle"' >> test1.ml
  $ echo '#!/usr/bin/env ocaml' > test2.ml
  $ echo 'let x = "first"' >> test2.ml
  $ echo '#!/usr/bin/env ocaml' > test3.ml
  $ echo 'let x = Test1.x ^ "-last";; let () = print_endline x' >> test3.ml
  $ $TEST_DIR/plugin_loader.exe test2.ml test1.ml test3.ml
  first-middle-last

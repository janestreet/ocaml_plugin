  $ setup

Loading file with a wrong name

  $ rm -f *.ml*
  $ touch invalid-name.ml
  $ $TEST_DIR/plugin_loader.exe invalid-name.ml |& matches 'not a valid ocaml'
  [1]

Loading mls that collide

  $ rm -f *.ml*
  $ touch Test.ml test.ml
  $ $TEST_DIR/plugin_loader.exe test.ml Test.ml |& matches 'Several implementations'
  [1]

Loadings mlis that collide

  $ rm -f *.ml*
  $ touch Test.mli test.mli
  $ $TEST_DIR/plugin_loader.exe test.mli Test.mli |& matches 'Several interfaces'
  [1]

Loading ml and mli with different basenames but same module name

  $ rm -f *.ml*
  $ echo 'let x = 2' > test.ml
  $ echo 'val x : string' > Test.mli
  $ $TEST_DIR/plugin_loader.exe test.ml Test.mli |& matches 'Signature mismatch'
  [1]

Loading ml and mli from different dirs

  $ rm -f *.ml*
  $ mkdir -p tmp_dir
  $ echo 'let x = 2' > test.ml
  $ echo 'let () = print_endline "different dirs ok"' >> test.ml
  $ echo 'val x : int' > test.mli
  $ echo 'val x : string' > tmp_dir/Test.mli
  $ $TEST_DIR/plugin_loader.exe test.ml tmp_dir/Test.mli |& matches 'Signature mismatch'
  [1]
  $ echo 'val x : int' > tmp_dir/Test.mli
  $ $TEST_DIR/plugin_loader.exe test.ml tmp_dir/Test.mli
  different dirs ok

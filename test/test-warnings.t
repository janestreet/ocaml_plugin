  $ setup

  $ rm -f *.ml*
  $ echo 'let x = "Test1.x"' >  test1.ml
  $ echo 'let y = "Test1.y"' >> test1.ml
  $ echo 'let x = Test1.x  ' >  test2.ml
  $ echo 'let y = Test1.y'   >> test2.ml
  $ echo 'let () = print_endline (x^y)' >> test2.ml
  $ $TEST_DIR/plugin_loader.exe test1.ml test2.ml
  Test1.xTest1.y

  $ rm -f *.ml*
  $ echo 'let x = "Test1.x"' >  test1.ml
  $ echo 'let y = "Test1.y"' >> test1.ml
  $ echo 'let x = Test1.x  ' >  test2.ml
  $ echo 'let () = print_endline x' >> test2.ml
  $ $TEST_DIR/plugin_loader.exe test1.ml test2.ml |& matches 'warning 32 \[unused-value-declaration\]): unused value y'
  [1]

  $ rm -f *.ml*
  $ echo 'let x = "Test1.x"' >  test1.ml
  $ echo 'let y = "Test1.y"' >> test1.ml
  $ echo 'val x : string'    >  test1.mli
  $ echo 'val y : string'    >> test1.mli
  $ echo 'let x = Test1.x  ' >  test2.ml
  $ echo 'let () = print_endline x' >> test2.ml
  $ $TEST_DIR/plugin_loader.exe test1.ml test2.ml
  Test1.x
  $ $TEST_DIR/plugin_loader.exe --warnings-in-utils test1.ml test2.ml |& matches 'warning 32 \[unused-value-declaration\]): unused value y'
  [1]

  $ rm -f *.ml*
  $ echo 'let x = "x"' >  test1.ml
  $ echo 'let y = "y"' >> test1.ml
  $ echo 'let () = print_endline y' >> test1.ml
  $ $TEST_DIR/plugin_loader.exe test1.ml |& matches 'warning 32 \[unused-value-declaration\]): unused value x'
  [1]

  $ rm -f *.ml*
  $ echo 'let x = "x"' >  test1.ml
  $ echo 'let y = "y"' >> test1.ml
  $ echo 'let () = print_endline y' >> test1.ml
  $ echo 'val x : string'    >  test1.mli
  $ echo 'val y : string'    >> test1.mli
  $ $TEST_DIR/plugin_loader.exe test1.ml
  y
  $ $TEST_DIR/plugin_loader.exe --warnings-in-utils test1.ml |& matches 'warning 32 \[unused-value-declaration\]): unused value y'
  [1]


  $ setup

Checking positions are good in error messages:

  $ echo 'let () = 1' > a.ml
  $ $TEST_DIR/plugin_loader.exe ./a.ml
  working_dir: $TESTTMP/* (glob)
  status: (Exit_non_zero 2)
  command: * (glob)
  File "$TESTTMP/a.ml", line 1, characters 9-10:
  Error: This expression has type int but an expression was expected of type
           unit
  
  [1]
  $ $TEST_DIR/plugin_loader.exe --ppx ./a.ml
  working_dir: $TESTTMP/* (glob)
  status: (Exit_non_zero 2)
  command: * (glob)
  File "$TESTTMP/a.ml", line 1, characters 9-10:
  Error: This expression has type int but an expression was expected of type
           unit
  
  [1]

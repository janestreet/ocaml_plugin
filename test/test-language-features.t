Testing that various language feature do work in plugins.

  $ setup

Lazy

  $ cat > test.ml <<EOF
  > let lazy x = lazy (print_endline "using lazy is fine"; 1+41)
  > let () = assert (x = 42)
  > EOF
  $ $TEST_DIR/plugin_loader.exe ./test.ml
  using lazy is fine

Recursive modules

  $ cat > test.ml <<EOF
  > module rec A : sig end = struct
  >  let () = print_endline "rec modules"
  > end and B : sig end = struct
  > end
  > EOF
  $ $TEST_DIR/plugin_loader.exe ./test.ml
  rec modules

Objects

  $ cat > test.ml <<EOF
  > let o = object
  >    method print = print_endline "objects ftw"
  > end
  > let () = o#print
  > EOF
  $ $TEST_DIR/plugin_loader.exe ./test.ml
  objects ftw

Generative functors

  $ cat > test.ml <<EOF
  > module F() = struct type _t print_endline "ok" end
  > include F()
  > EOF
  $ $TEST_DIR/plugin_loader.exe ./test.ml
  ok

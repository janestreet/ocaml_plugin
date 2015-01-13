It used to be the case that in the default mode where we get warnings for every unused
value in modules without interfaces, the inferred interfaces would not be in the scope
of hand written interfaces.

  $ setup
  $ echo 'type t = string' > a.ml
  $ echo 'type t = A.t' > b.mli
  $ echo 'type t = A.t' > b.ml
  $ echo 'let () = print_endline "ok"' >> b.ml
  $ $TEST_DIR/plugin_loader.exe a.ml b.mli b.ml
  ok

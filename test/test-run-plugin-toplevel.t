  $ setup

  $ cat > test.ml <<EOF
  > let () = Core.Std.Caml.Printf.printf "%b\n%!"
  >   (Async.Std.Thread_safe.am_holding_async_lock ())
  > EOF
  $ $TEST_DIR/plugin_loader.exe test.ml
  true
  $ $TEST_DIR/plugin_loader.exe --toplevel-outside-of-async test.ml
  false

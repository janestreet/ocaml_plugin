  $ setup

  $ cat > test.ml <<EOF
  > let () = Core.Std.Printf.printf "%b\n%!"
  >   (Async.Std.Thread_safe.am_holding_async_lock ())
  > EOF
  $ $TEST_DIR/plugin_loader.exe test.ml
  true

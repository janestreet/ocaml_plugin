  $ setup

  $ cat > test.ml <<EOF
  > let () = Core.Printf.printf "%b\n%!"
  >   (Async.Thread_safe.am_holding_async_lock ())
  > EOF
  $ $TEST_DIR/plugin_loader.exe test.ml
  true

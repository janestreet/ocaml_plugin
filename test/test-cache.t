  $ setup

Loading the same ml file several times with a cache

  $ function cache_size { find cache -type f -name '*.cmxs' | wc -l; }

  $ echo 'print_endline "loaded"' > test.ml
  $ $TEST_DIR/plugin_loader.exe --cache 'test.ml | test.ml'
  loaded
  loaded
  $ cache_size
  1

  $ $TEST_DIR/plugin_loader.exe --cache 'test.ml | test.ml'
  loaded
  loaded
  $ cache_size
  1

Checking that we really are using the cache

  $ cmxs_before="$(find cache -type f -name '*.cmxs')"

  $ echo 'print_endline "loaded2"' > test2.ml
  $ $TEST_DIR/plugin_loader.exe --cache 'test2.ml'
  loaded2
  $ cache_size
  2

  $ cmxs_after="$(find cache -type f -name '*.cmxs')"
  $ for i in $cmxs_after; do if [ "$i" != "$cmxs_before" ]; then cp "$i" "$cmxs_before"; fi; done
  $ $TEST_DIR/plugin_loader.exe --cache test.ml
  loaded2

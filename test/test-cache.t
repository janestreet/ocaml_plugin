  $ setup

Loading the same ml file several times with a cache.

  $ function cache_size { find cache -type f -name '*.cmxs' | wc -l; }

  $ echo 'let () = print_endline "loaded"' > test.ml
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

Check that plugin are indexed by basenames.

  $ mkdir -p plugins
  $ cp test.ml plugins/.
  $ $TEST_DIR/plugin_loader.exe --cache 'plugins/test.ml | plugins/test.ml'
  loaded
  loaded
  $ cache_size
  1

Checking that we really are using the cache.

  $ cmxs_before="$(find cache -type f -name '*.cmxs')"

  $ echo 'let () = print_endline "loaded2"' > test2.ml
  $ $TEST_DIR/plugin_loader.exe --cache 'test2.ml'
  loaded2
  $ cache_size
  2

  $ cmxs_after="$(find cache -type f -name '*.cmxs')"
  $ for i in $cmxs_after; do if [ "$i" != "$cmxs_before" ]; then cp "$i" "$cmxs_before"; fi; done
  $ $TEST_DIR/plugin_loader.exe --cache test.ml
  loaded2

Check the heuristic to pick what old cache to clean.

  $ cache_info="cache/cmxs-cache/cache-info.sexp"
  $ cat $cache_info | grep -q "$TESTTMP/test.ml"

  $ echo 'let () = print_endline "plugins/loaded"' > plugins/test.ml
  $ $TEST_DIR/plugin_loader.exe --cache-size 2 --cache 'plugins/test.ml | plugins/test.ml'
  plugins/loaded
  plugins/loaded
  $ cache_size
  2

  $ cache_info="cache/cmxs-cache/cache-info.sexp"
  $ cat $cache_info | grep -q "$TESTTMP/test.ml"
  [1]

  $ setup

Checking that archive is really persistent

  $ function ocamlopt_stat {
  >     stat -c %z cache/compiler/ocamlopt.opt
  > }

  $ echo 'let () = print_endline "persistent"' > test1.ml
  $ $TEST_DIR/plugin_loader.exe --persistent-archive test1.ml
  persistent
  $ grep 'archive_digest fake-digest' cache/compiler/archive-info.sexp
  [1]
  $ compiler_stat=$(ocamlopt_stat)
  $ $TEST_DIR/plugin_loader.exe --persistent-archive test1.ml
  persistent
  $ compiler_stat2=$(ocamlopt_stat)
  $ [ "$compiler_stat" = "$compiler_stat2" ]
  $ sed -r -i -e 's/cmi [0-9a-f]{32}/cmi aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa/' cache/compiler/archive-info.sexp
  $ $TEST_DIR/plugin_loader.exe --persistent-archive test1.ml
  persistent
  $ compiler_stat3=$(ocamlopt_stat)
  $ [ "$compiler_stat2" != "$compiler_stat3" ]

Check that if the extracted archived is corrupted (missing ppx.exe here), we recover:

  $ rm cache/compiler/*.exe
  $ $TEST_DIR/plugin_loader.exe --persistent-archive test1.ml
  persistent

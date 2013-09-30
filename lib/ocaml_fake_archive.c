#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

CAMLprim value ocaml_plugin_archive (value unit __attribute__ ((unused)))
{
  return(caml_copy_string("dummy"));
}

CAMLprim value ocaml_plugin_archive_digest (value unit __attribute__ ((unused)))
{
  return(caml_copy_string("dummy"));
}

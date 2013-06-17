#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

CAMLprim value ocaml_plugin_archive (value unit __attribute__ ((unused)))
{
  char v[] = "dummy";

  return(caml_copy_string(v));
}

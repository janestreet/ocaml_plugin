#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/bigarray.h>

static char s[] = "dummy";

/* Here and in the generated .c files, we must put CAML_BA_EXTERNAL instead of
   CAML_BA_MANAGED because the C string cannot be freed. */
CAMLprim CAMLweakdef value ocaml_plugin_archive (value unit __attribute__ ((unused)))
{
  intnat dim = 5;
  int flags = CAML_BA_UINT8 | CAML_BA_C_LAYOUT | CAML_BA_EXTERNAL;
  return caml_ba_alloc(flags, 1, s, &dim);
}

CAMLprim CAMLweakdef value ocaml_plugin_archive_metadata (value unit __attribute__ ((unused)))
{
  return(caml_copy_string(s));
}

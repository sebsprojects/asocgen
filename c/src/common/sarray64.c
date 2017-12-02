#include "sarray64.h"

#ifdef STRUCT_HACK // -------------------------------------------------------

Array_uint16 *aui64_alloc(uint32_t size) {
  Array_uint64 *array = malloc(sizeof(Array_uint64) + size * sizeof(uint64_t));
  array->size = size;
  array->data = (uint64_t*) array + sizeof(Array_uint64);
  return array;
}

void aui64_free(Array_uint64 *array) {
  free(array);
}

#else // --------------------------------------------------------------------

Array_uint64 *aui64_alloc(uint32_t size) {
  Array_uint64 *array = malloc(sizeof(Array_uint64));
  array->size = size;
  array->data = malloc(size * sizeof(uint64_t));
  return array;
}


void aui64_free(Array_uint64 *array) {
  free(array->data);
  free(array);
}

#endif // -------------------------------------------------------------------

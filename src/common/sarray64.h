#ifndef SARRAY64
#define SARRAY64

#include "common.h"

/*
  Array_uint64 is a thin wrapper around a raw uint64_t C-style array.

  For more information, see the uint16_t version in array16.h
*/

struct Array_uint64 {
  uint32_t size;
  uint64_t *data;
};
typedef struct Array_uint64 Array_uint64;


// --------------------------------------------------------------------------
// Alloc / Free
// --------------------------------------------------------------------------

Array_uint64 *aui64_alloc(uint32_t size);
void aui64_free(Array_uint64 *array);


// --------------------------------------------------------------------------
// Inline Functions
// --------------------------------------------------------------------------

inline uint64_t *aui64_at(Array_uint64 *array, uint32_t index) {
#ifdef BOUNDS_CHECK
  if(index >= array->size) {
    fprintf(stderr, "error: Array_uint64 out of bounds: size=%u, index=%u \n",
            array->size, index);
    exit(1);
  }
#endif
  return &array->data[index];
}

#endif

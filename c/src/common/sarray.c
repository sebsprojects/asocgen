#include <stdlib.h>
#include <stdio.h>

#include "sarray.h"


Array_uint8 *allocArray_uint8(uint32_t size) {
  Array_uint8 *array = malloc(sizeof(Array_uint8) + size * sizeof(uint8_t));
  array->size = size;
  return array;
}

void freeArray_uint8(Array_uint8 *array) {
  free(array);
}

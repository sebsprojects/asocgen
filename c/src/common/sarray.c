#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

#include "sarray.h"


#ifdef STRUCT_HACK

Array_uint8 *allocArray_uint8(uint32_t size) {
  Array_uint8 *array = malloc(sizeof(Array_uint8) + size * sizeof(uint8_t));
  array->size = size;
  array->data = (uint8_t*) array + sizeof(Array_uint8);
  return array;
}

Array_uint16 *allocArray_uint16(uint32_t size) {
  Array_uint16 *array = malloc(sizeof(Array_uint16) + size * sizeof(uint16_t));
  array->size = size;
  array->data = (uint16_t*) array + sizeof(Array_uint16);
  return array;
}

Array_uint16 *allocArrayN_uint16(uint32_t b, uint16_t arg1, ...) {
  return 0;
}

void freeArray_uint8(Array_uint8 *array) {
  free(array);
}

void freeArray_uint16(Array_uint16 *array) {
  free(array);
}

#else

Array_uint8 *allocArray_uint8(uint32_t size) {
  Array_uint8 *array = malloc(sizeof(Array_uint8));
  array->size = size;
  array->data = malloc(size * sizeof(uint8_t));
  return array;
}

Array_uint16 *allocArray_uint16(uint32_t size) {
  Array_uint16 *array = malloc(sizeof(Array_uint16));
  array->size = size;
  array->data = malloc(size * sizeof(uint16_t));
  return array;
}

void freeArray_uint8(Array_uint8 *array) {
  free(array->data);
  free(array);
}

void freeArray_uint16(Array_uint16 *array) {
  free(array->data);
  free(array);
}

#endif

void shrink_uint8(Array_uint8 *array, uint32_t newSize) {
#ifdef BOUNDS_CHECK
  if(array->size < newSize) {
    fprintf(stderr, "error: Array_uint8 shrink: size=%u, newsize=%u \n",
	    array->size, newSize);
    exit(1);
  }
#endif
  array->size = newSize;
}

void shrink_uint16(Array_uint16 *array, uint32_t newSize) {
#ifdef BOUNDS_CHECK
  if(array->size < newSize) {
    fprintf(stderr, "error: Array_uint16 shrink: size=%u, newsize=%u \n",
	    array->size, newSize);
    exit(1);
  }
#endif
  array->size = newSize;
}

Array_uint8 *allocArrayN_uint8(uint32_t n, ...) {
  Array_uint8 *array = allocArray_uint8(n);
  va_list args;
  va_start(args, n);
  uint32_t i;
  for(i = 0; i < n; i++) {
    array->data[i] = va_arg(args, int);
  }
  va_end(args);
  return array;
}

Array_uint16 *allocArrayN_uint16(uint32_t n, ...) {
  Array_uint16 *array = allocArray_uint16(n);
  va_list args;
  va_start(args, n);
  uint32_t i;
  for(i = 0; i < n; i++) {
    array->data[i] = va_arg(args, int);
  }
  va_end(args);
  return array;
}

Array_uint16 *allocArray1_uint16(uint16_t e1) {
  Array_uint16 *array = allocArray_uint16(1);
  array->data[0] = e1;
  return array;
}
Array_uint16 *allocArray2_uint16(uint16_t e1, uint16_t e2) {
  Array_uint16 *array = allocArray_uint16(2);
  array->data[0] = e1;
  array->data[1] = e2;
  return array;
}
Array_uint16 *allocArray3_uint16(uint16_t e1, uint16_t e2, uint16_t e3) {
  Array_uint16 *array = allocArray_uint16(3);
  array->data[0] = e1;
  array->data[1] = e2;
  array->data[2] = e3;
  return array;
}
Array_uint16 *allocArray4_uint16(uint16_t e1, uint16_t e2,
                                 uint16_t e3, uint16_t e4) {
  Array_uint16 *array = allocArray_uint16(4);
  array->data[0] = e1;
  array->data[1] = e2;
  array->data[2] = e3;
  array->data[3] = e4;
  return array;
}
Array_uint16 *allocArray5_uint16(uint16_t e1, uint16_t e2, uint16_t e3,
                                 uint16_t e4, uint16_t e5) {
  Array_uint16 *array = allocArray_uint16(5);
  array->data[0] = e1;
  array->data[1] = e2;
  array->data[2] = e3;
  array->data[3] = e4;
  array->data[4] = e5;
  return array;
}

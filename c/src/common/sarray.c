#include <stdlib.h>
#include <stdio.h>

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

void freeArray_uint8(Array_uint8 *array) {
  free(array);
}

void freeArray_uint16(Array_uint16 *array) {
  free(array);
}

Array_uint8 *setArray_uint8(void *ptr, uint32_t size) {
  Array_uint8 *array = (Array_uint8*) ptr;
  array->size = size;
  return array;
}

Array_uint16 *setArray_uint16(void *ptr, uint32_t size) {
  Array_uint16 *array = (Array_uint16*) ptr;
  array->size = size;
  return array;
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

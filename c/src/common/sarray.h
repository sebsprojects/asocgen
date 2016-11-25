#ifndef SARRAY
#define SARRAY

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>


struct Array_uint8 {
  uint32_t size;
  uint8_t *data;
};
typedef struct Array_uint8 Array_uint8;

struct Array_uint16 {
  uint32_t size;
  uint16_t *data;
};
typedef struct Array_uint16 Array_uint16;

Array_uint8 *allocArray_uint8(uint32_t size);
Array_uint16 *allocArray_uint16(uint32_t size);

void freeArray_uint8(Array_uint8 *array);
void freeArray_uint16(Array_uint16 *array);

#ifdef STRUCT_HACK
Array_uint8 *setArray_uint8(void *ptr, uint32_t size);
Array_uint16 *setArray_uint16(void *ptr, uint32_t size);
#endif

inline uint8_t *at_uint8(Array_uint8 *array, uint32_t index) {
  #ifdef BOUNDS_CHECK
  if(index >= array->size) {
    fprintf(stderr, "error: Array_uint8 out of bounds: size=%u, index=%u \n",
	    array->size, index);
    exit(1);
  }
  #endif
  return &array->data[index];
}

inline uint16_t *at_uint16(Array_uint16 *array, uint32_t index) {
  #ifdef BOUNDS_CHECK
  if(index >= array->size) {
    fprintf(stderr, "error: Array_uint16 out of bounds: size%u, index%u \n",
            array->size, index);
    exit(1);
  }
  #endif
  return &array->data[index];
}

#endif

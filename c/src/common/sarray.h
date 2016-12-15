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
#ifdef BOUNDS_CHECK
  uint32_t _allocSize;
#endif
  uint32_t size;
  uint16_t *data;
};
typedef struct Array_uint16 Array_uint16;

struct Array_uint64 {
  uint32_t size;
  uint64_t *data;
};
typedef struct Array_uint64 Array_uint64;

Array_uint8 *allocArray_uint8(uint32_t size);
Array_uint16 *allocArray_uint16(uint32_t size);
Array_uint64 *allocArray_uint64(uint32_t size);

Array_uint8 *allocArrayN_uint8(uint32_t n, ...);

Array_uint16 *allocArray1_uint16(uint16_t e1);
Array_uint16 *allocArray2_uint16(uint16_t e1, uint16_t e2);
Array_uint16 *allocArray3_uint16(uint16_t e1, uint16_t e2, uint16_t e3);
Array_uint16 *allocArray4_uint16(uint16_t e1, uint16_t e2,
                                   uint16_t e3, uint16_t e4);
Array_uint16 *allocArray5_uint16(uint16_t e1, uint16_t e2, uint16_t e3,
                                   uint16_t e4, uint16_t e5);
Array_uint16 *allocArrayN_uint16(uint32_t n, ...);

void freeArray_uint8(Array_uint8 *array);
void freeArray_uint16(Array_uint16 *array);
void freeArray_uint64(Array_uint64 *array);

void grow_uint16(Array_uint16 *array, uint32_t newSize);
void shrink_uint16(Array_uint16 *array, uint32_t newSize);

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
    fprintf(stderr, "error: Array_uint16 out of bounds: size=%u, index=%u \n",
            array->size, index);
    exit(1);
  }
#endif
  return &array->data[index];
}

inline uint64_t *at_uint64(Array_uint64 *array, uint32_t index) {
#ifdef BOUNDS_CHECK
  if(index >= array->size) {
    fprintf(stderr, "error: Array_uint64 out of bounds: size=%u, index=%u \n",
            array->size, index);
    exit(1);
  }
#endif
  return &array->data[index];
}

inline void fillArray_uint16(Array_uint16 *array, uint16_t val) {
  uint32_t i;
  for(i = 0; i < array->size; i++) {
    *at_uint16(array, i) = val;
  }
}

// For now: stupid linear search, no error on fail
inline uint16_t indexof_uint16(Array_uint16 *array, uint16_t ele) {
  uint32_t i;
  for(i = 0; i < array->size; i++) {
    if(*at_uint16(array, i) == ele) return i;
  }
  return 0xffff;
}

#endif

#include "sarray.h"

#include <stdarg.h>


#ifdef STRUCT_HACK

Array_uint8 *allocArray_uint8(uint32_t size) {
  Array_uint8 *array = malloc(sizeof(Array_uint8) + size * sizeof(uint8_t));
  array->size = size;
  array->data = (uint8_t*) array + sizeof(Array_uint8);
  return array;
}

Array_uint16 *allocArray_uint16(uint32_t size) {
  Array_uint16 *array = malloc(sizeof(Array_uint16) + size *
                               sizeof(uint16_t));
#ifdef BOUNDS_CHECK
  array->_allocSize = size;
#endif
  array->size = size;
  array->data = (uint16_t*) array + sizeof(Array_uint16);
  return array;
}

Array_uint16 *allocArray_uint64(uint32_t size) {
  Array_uint64 *array = malloc(sizeof(Array_uint64) + size *
                               sizeof(uint64_t));
  array->size = size;
  array->data = (uint64_t*) array + sizeof(Array_uint64);
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
#ifdef BOUNDS_CHECK
  array->_allocSize = size;
#endif
  array->size = size;
  array->data = malloc(size * sizeof(uint16_t));
  return array;
}

Array_uint64 *allocArray_uint64(uint32_t size) {
  Array_uint64 *array = malloc(sizeof(Array_uint64));
  array->size = size;
  array->data = malloc(size * sizeof(uint64_t));
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

void freeArray_uint64(Array_uint64 *array) {
  free(array->data);
  free(array);
}

#endif


Array_uint16 *copyArray_uint16(Array_uint16 *array) {
  Array_uint16 *copy = allocArray_uint16(array->size);
  memcpy(copy->data, array->data, array->size * sizeof(uint16_t));
  return copy;
}

void copyIntoArray_uint16(Array_uint16 *to, Array_uint16* from) {
#ifdef BOUNDS_CHECK
  if(to->size < from->size) {
    fprintf(stderr,
            "error: Array_uint16 copyIntoArray: from->size=%u, to->size=%u",
            from->size, to->size);
    exit(1);
  }
#endif
  memcpy(to->data, from->data, from->size *sizeof(uint16_t));
}

void grow_uint16(Array_uint16 *array, uint32_t newSize) {
#ifdef BOUNDS_CHECK
  if(array->size > array->_allocSize) {
    fprintf(stderr, "error: Array_uint16 grow: allocsize=%u, newsize=%u \n",
	    array->_allocSize, newSize);
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

bool areEqualArrays_uint16(Array_uint16 *a, Array_uint16 *b) {
  if(a->size != b->size) {
    return 0;
  }
  uint32_t i;
  for(i = 0; i < a->size; i++) {
    if(a->data[i] != b->data[i]) { // No bounds check needed in any case
      return 0;
    }
  }
  return 1;
}

bool areEqualSets_uint16(Array_uint16 *a, Array_uint16 *b) {
  return isSubset_uint16(a, b) && isSubset_uint16(b, a);
}

bool isSubset_uint16(Array_uint16 *sub, Array_uint16 *set) {
  uint32_t i;
  for(i = 0; i < sub->size; i++) {
    if(indexof_uint16(set, *at_uint16(sub, i)) >= set->size) {
      return 0;
    }
  }
  return 1;
}

int32_t lessThanCompare_uint16(const void *x, const void *y) {
  uint16_t xx = *(uint16_t*) x;
  uint16_t yy = *(uint16_t*) y;
  if(xx < yy) {
    return -1;
  }
  if(xx > yy) {
    return 1;
  }
  return 0;
}

void sort_uint16(Array_uint16 *a, uint32_t start, uint32_t end) {
#ifdef BOUNDS_CHECK
  if(end >= a->size || start >= a->size) {
    fprintf(stderr, "error: Array_uint16 sort: size=%u, start=%u, end=%u \n",
	    a->size, start, end);
    exit(1);
  }
#endif
  uint16_t *base = &a->data[start];
  uint32_t num = end - start + 1;
  uint32_t size = sizeof(uint16_t);
  qsort(base, num, size, lessThanCompare_uint16);
}

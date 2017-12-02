#include "sarray16.h"

#include <stdarg.h>

#ifdef STRUCT_HACK // -------------------------------------------------------

Array_uint16 *aui16_alloc(uint32_t size) {
  Array_uint16 *array = malloc(sizeof(Array_uint16) + size *sizeof(uint16_t));
#ifdef BOUNDS_CHECK
  array->_allocSize = size;
#endif
  array->size = size;
  array->data = (uint16_t*) array + sizeof(Array_uint16);
  return array;
}

void aui16_free(Array_uint16 *array) {
  free(array);
}

#else // --------------------------------------------------------------------

Array_uint16 *aui16_alloc(uint32_t size) {
  Array_uint16 *array = malloc(sizeof(Array_uint16));
#ifdef BOUNDS_CHECK
  array->_allocSize = size;
#endif
  array->size = size;
  array->data = malloc(size * sizeof(uint16_t));
  return array;
}

void aui16_free(Array_uint16 *array) {
  free(array->data);
  free(array);
}

#endif // -------------------------------------------------------------------

Array_uint16 *aui16_allocN(uint32_t n, ...) {
  Array_uint16 *array = aui16_alloc(n);
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
  return aui16_allocN(1, e1);
}
Array_uint16 *allocArray2_uint16(uint16_t e1, uint16_t e2) {
  return aui16_allocN(2, e1, e2);
}
Array_uint16 *allocArray3_uint16(uint16_t e1, uint16_t e2, uint16_t e3) {
  return aui16_allocN(3, e1, e2, e3);
}
Array_uint16 *allocArray4_uint16(uint16_t e1, uint16_t e2,
                                 uint16_t e3, uint16_t e4) {
  return aui16_allocN(4, e1, e2, e3, e4);
}
Array_uint16 *allocArray5_uint16(uint16_t e1, uint16_t e2, uint16_t e3,
                                 uint16_t e4, uint16_t e5) {
  return aui16_allocN(5, e1, e2, e3, e4, e5);
}

// -------------------------------------------------------------------------

void aui16_grow(Array_uint16 *array, uint32_t newSize) {
#ifdef BOUNDS_CHECK
  if(array->size > array->_allocSize) {
    fprintf(stderr, "error: Array_uint16 grow: allocsize=%u, newsize=%u \n",
	    array->_allocSize, newSize);
    exit(1);
  }
#endif
  array->size = newSize;
}

void aui16_shrink(Array_uint16 *array, uint32_t newSize) {
#ifdef BOUNDS_CHECK
  if(array->size < newSize) {
    fprintf(stderr, "error: Array_uint16 shrink: size=%u, newsize=%u \n",
	    array->size, newSize);
    exit(1);
  }
#endif
  array->size = newSize;
}

Array_uint16 *aui16_copy(Array_uint16 *array) {
  Array_uint16 *copy = aui16_alloc(array->size);
  uint32_t i;
  for(i = 0; i < array->size; i++) {
    *aui16_at(copy, i) = *aui16_at(array, i);
  }
  return copy;
}

void aui16_copyInto(Array_uint16 *to, Array_uint16* from) {
#ifdef BOUNDS_CHECK
  if(to->size < from->size) {
    fprintf(stderr,
            "error: Array_uint16 copyIntoArray: from->size=%u, to->size=%u",
            from->size, to->size);
    exit(1);
  }
#endif
  uint32_t i;
  for(i = 0; i < from->size; i++) {
    *aui16_at(to, i) = *aui16_at(from, i);
  }
}


void aui16_sort(Array_uint16 *a, uint32_t start, uint32_t end) {
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
  int32_t lessThanCompare(const void *x, const void *y);
  qsort(base, num, size, lessThanCompare);
}

bool aui16_areEqualArrays(Array_uint16 *a, Array_uint16 *b) {
  if(a->size != b->size) {
    return 0;
  }
  uint32_t i;
  for(i = 0; i < a->size; i++) {
    if(*aui16_at(a, i) != *aui16_at(b, i)) {
      return 0;
    }
  }
  return 1;
}

bool aui16_haveEqualContent(Array_uint16 *a, Array_uint16 *b) {
  fprintf(stderr, "error: aui16_haveEqualContent is not implemented yet\n");
  exit(1);
}

bool aui16_hasDuplicates(Array_uint16 *array) {
  uint32_t i, j;
  uint16_t ele;
  for(i = 0; i < array->size; i++) {
    ele = *aui16_at(array, i);
    for(j = 0; j < array->size; j++) {
      if(j == i) {
        continue;
      }
      if(ele == *aui16_at(array, j)) {
        return 0;
      }
    }
  }
  return 1;
}

// -------------------------------------------------------------------------

bool aui16_isProperSet(Array_uint16 *array) {
  return !aui16_hasDuplicates(array);
}

bool aui16_areEqualSets(Array_uint16 *a, Array_uint16 *b) {
  return aui16_isSubset(a, b) && aui16_isSubset(b, a);
}

bool aui16_isSubset(Array_uint16 *sub, Array_uint16 *set) {
  uint32_t i;
  for(i = 0; i < sub->size; i++) {
    if(aui16_indexOf(set, *aui16_at(sub, i)) >= set->size) {
      return 0;
    }
  }
  return 1;
}

// -------------------------------------------------------------------------

void aui16_sprint(char *string, Array_uint16 *arr) {
  uint32_t n = arr->size;
  uint32_t i;
  string[0] = '\0';

  sprintf(string, "[ ");
  for(i = 0; i < n; i++) {
    padStringForInt(string, *aui16_at(arr, i));
    sprintf(string + strlen(string), "%u", *aui16_at(arr, i));
    if(i < n - 1) sprintf(string + strlen(string), ", ");
    if((i + 1) % 10 == 0) sprintf(string + strlen(string), "\n  ");
  }
  sprintf(string + strlen(string), " ]\n");
}

void aui16_sprintSquare(char *string, Array_uint16 *arr, uint32_t sq) {
  uint32_t i, j;
  uint8_t ele;
  string[0] = '\0';

  for(i = 0; i < sq; i++) {
    for(j = 0; j < sq; j++) {
      ele = *aui16_at(arr, i * sq + j);
      sprint_uint(string, ele);
    }
    sprintf(string + strlen(string), "\n");
  }
}

void aui16_print(char *string, Array_uint16 *arr) {
  aui16_sprint(string, arr);
  printf(string);
}

// -------------------------------------------------------------------------

int32_t lessThanCompare(const void *x, const void *y) {
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

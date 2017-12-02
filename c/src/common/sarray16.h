#ifndef SARRAY16
#define SARRAY16

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include "common.h"

/*
  Array_uint16 is a thin wrapper around a raw uint16_t C-style array. It adds
  a size attribute and optional BOUNDS_CHECKs. It is possible to tightly
  pack the struct by setting STRUCT_HACK.
  If BOUNDS_CHECK is not set, then there should be no overhead to direct
  array access.

  It is possible to shrink (and grow again) an array (up to its original
  size).

  Various utility functions are - somewhat inefficiently - implemented.

  Array_uint16 also provides various set functions and can act as a set. This
  is not enforces however.
*/

struct Array_uint16 {
#ifdef BOUNDS_CHECK
  uint32_t _allocSize;
#endif
  uint32_t size;
  uint16_t *data;
};
typedef struct Array_uint16 Array_uint16;


// --------------------------------------------------------------------------
// Alloc / Free
// --------------------------------------------------------------------------

Array_uint16 *aui16_alloc(uint32_t size);
Array_uint16 *aui16_alloc1(uint16_t e1);
Array_uint16 *aui16_alloc2(uint16_t e1, uint16_t e2);
Array_uint16 *aui16_alloc3(uint16_t e1, uint16_t e2, uint16_t e3);
Array_uint16 *aui16_alloc4(uint16_t e1, uint16_t e2,
                                   uint16_t e3, uint16_t e4);
Array_uint16 *aui16_alloc5(uint16_t e1, uint16_t e2, uint16_t e3,
                                   uint16_t e4, uint16_t e5);
Array_uint16 *aui16_allocN(uint32_t n, ...);

void aui16_free(Array_uint16 *array);


// --------------------------------------------------------------------------
// Management
// --------------------------------------------------------------------------

void aui16_grow(Array_uint16 *array, uint32_t newSize);
void aui16_shrink(Array_uint16 *array, uint32_t newSize);

Array_uint16 *aui16_copy(Array_uint16 *array);
void aui16_copyInto(Array_uint16 *to, Array_uint16* from);

/*
  Sort between array between [start, end]. Uses qsort from stdlib
 */
void aui16_sort(Array_uint16 *a, uint32_t start, uint32_t end);

/*
  Only true if a[i] == b[i] for equal sized arrays and i = 0..a->size
 */
bool aui16_areEqualArrays(Array_uint16 *a, Array_uint16 *b);

/*
  Disregards ordering. Still requires equal sizes
 */
bool aui16_haveEqualContent(Array_uint16 *a, Array_uint16 *b);

bool aui16_hasDuplicates(Array_uint16 *array);


// --------------------------------------------------------------------------
// Set Functions
// --------------------------------------------------------------------------

bool aui16_isProperSet(Array_uint16 *a);

/*
  Disregards ordering and duplicates
 */
bool aui16_areEqualSets(Array_uint16 *a, Array_uint16 *b);

/*
  Disregards ordering and duplicates
 */
bool aui16_isSubset(Array_uint16 *sub, Array_uint16 *set);


// --------------------------------------------------------------------------
// Printing
// --------------------------------------------------------------------------

void aui16_sprint(char *pstring, Array_uint16 *arr);
void aui16_sprintSquare(char *pstring, Array_uint16 *arr, uint32_t sq);
void aui16_print(char *pstring, Array_uint16 *arr);


// --------------------------------------------------------------------------
// Inline Functions
// --------------------------------------------------------------------------

inline uint16_t *aui16_at(Array_uint16 *array, uint32_t index) {
#ifdef BOUNDS_CHECK
  if(index >= array->size) {
    fprintf(stderr, "error: Array_uint16 out of bounds: size=%u, index=%u \n",
            array->size, index);
    exit(1);
  }
#endif
  return &array->data[index];
}

inline void aui16_fill(Array_uint16 *array, uint16_t val) {
  uint32_t i;
  for(i = 0; i < array->size; i++) {
    *aui16_at(array, i) = val;
  }
}

// For now: stupid linear search, no error on fail
inline uint16_t aui16_indexOf(Array_uint16 *array, uint16_t ele) {
  uint32_t i;
  for(i = 0; i < array->size; i++) {
    if(*aui16_at(array, i) == ele) return i;
  }
  return 0xffff;
}

#endif

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

struct Array_uint8;
Array_uint16 *aui16_allocFromAui8(struct Array_uint8 *aui8);

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

uint16_t aui16_getMax(Array_uint16 *array);
uint16_t aui16_getMin(Array_uint16 *array);


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

/*
  Sorints the interval [indFrom, indTo) of the array. Pads up to digitPad
  digits.
  If startBracket == 1, then the pstring starts with "[ ".
  If endBracket == 1, then the pstring ends with " ]".
 */
void aui16_sprintLine(Array_uint16 *arr, char *pstring, uint32_t indFrom,
                      uint32_t indTo, uint32_t indent, uint32_t digitPad);

/*
  sprints the array with maximal width = width. Pads the last line to full
  width (aligned).
 */
void aui16_sprintToNumWithPad(Array_uint16 *arr, char *pstring,
                               uint32_t elePerLine, uint32_t indent,
                               uint32_t digitPad);

/*
  Take the maximum value of arr for digit pad calculation
 */
void aui16_sprintToNum(Array_uint16 *arr, char *pstring,
                       uint32_t elePerLine, uint32_t indent);
void aui16_sprintToWidth(Array_uint16 *arr, char *pstring,
                         uint32_t elePerLine, uint32_t indent);


/*
  sprints with width = 80 and indent = 0
 */
void aui16_sprintDefault(Array_uint16 *arr, char *pstring);

/*
  sprints the array as a square matrix (row-wise). If array size is not a
  square number, then then the next biggest integer is taken as square size
 */
void aui16_sprintSquare(Array_uint16 *arr, char *pstring, uint32_t indent);

void aui16_printToNum(Array_uint16 *arr, uint32_t elePerLine,uint32_t indent);
void aui16_printToWidth(Array_uint16 *arr, uint32_t width, uint32_t indent);
void aui16_printDefault(Array_uint16 *arr);
void aui16_printSquare(Array_uint16 *arr, uint32_t indent);


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

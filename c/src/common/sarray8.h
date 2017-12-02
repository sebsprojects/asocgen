#ifndef SARRAY8
#define SARRAY8

#include <stdio.h>
#include "common.h"

/*
  Array_uint16 is a thin wrapper around a raw uint8_t C-style array.

  For more information, see the uint16_t version in array16.h
*/

struct Array_uint8 {
  uint32_t size;
  uint8_t *data;
};
typedef struct Array_uint8 Array_uint8;


// --------------------------------------------------------------------------
// Alloc / Free
// --------------------------------------------------------------------------

Array_uint8 *aui8_alloc(uint32_t size);
Array_uint8 *aui8_allocN(uint32_t n, ...);

void aui8_free(Array_uint8 *array);


// --------------------------------------------------------------------------
// Printing
// --------------------------------------------------------------------------

void aui8_sprint(char *string, Array_uint8 *arr);
void aui8_sprintSquare(char *string, Array_uint8 *arr, uint32_t sq);
void aui8_print(char *string, Array_uint8 *arr);


// --------------------------------------------------------------------------
// Inline Functions
// --------------------------------------------------------------------------

inline uint8_t *aui8_at(Array_uint8 *array, uint32_t index) {
#ifdef BOUNDS_CHECK
  if(index >= array->size) {
    fprintf(stderr, "error: Array_uint8 out of bounds: size=%u, index=%u \n",
	    array->size, index);
    exit(1);
  }
#endif
  return &array->data[index];
}

#endif

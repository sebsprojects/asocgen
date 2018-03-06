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

/*
  See sarray16.h for documentation. Array_uint8 are simply converted to
  Array_uint16 and then are printed as such.
 */

void aui8_sprintToNum(Array_uint8 *arr, char *pstring,
                      uint32_t elePerLine, uint32_t indent);
void aui8_sprintToWidth(Array_uint8 *arr, char *pstring,
                        uint32_t elePerLine, uint32_t indent);
void aui8_sprintDefault(Array_uint8 *arr, char *pstring);
void aui8_sprintSquare(Array_uint8 *arr, char *pstring, uint32_t indent);

void aui8_printToNum(Array_uint8 *arr, uint32_t elePerLine,uint32_t indent);
void aui8_printToWidth(Array_uint8 *arr, uint32_t width, uint32_t indent);
void aui8_printDefault(Array_uint8 *arr);
void aui8_printSquare(Array_uint8 *arr, uint32_t indent);


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

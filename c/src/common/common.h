#ifndef COMMON
#define COMMON

#include <stdint.h>

#include "sarray.h"


typedef int32_t bool;

/* Requires at most (6 * arr->size + 10) bytes of string */
void sprintArray_uint8(char *string, Array_uint8* arr);

void printArray_uint8(char *string, Array_uint8* arr);

/* Requires at most ( ? ) bytes of string */
void sprintArraySquare_uint8(char *string, Array_uint8* arr, uint32_t sq);

#endif

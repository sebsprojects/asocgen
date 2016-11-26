#ifndef COMMON
#define COMMON

#include <stdint.h>

#include "sarray.h"


typedef int32_t bool;

void printArray_uint8(char *string, Array_uint8 *arr);
void printArray_uint16(char *string, Array_uint16 *arr);

void sprintArray_uint8(char *string, Array_uint8 *arr); // *6 + 10
void sprintArray_uint16(char *string, Array_uint16 *arr); // *6 + 10

void sprintArraySquare_uint8(char *string, Array_uint8 *arr, uint32_t sq);
void sprintArraySquare_uint16(char *string, Array_uint16 *arr, uint32_t sq);

void printError(char *error);

inline uint8_t getRowIndex(uint32_t n, uint32_t ind) {
  return ind / n;
}

inline uint8_t getColIndex(uint32_t n, uint32_t ind) {
  return ind % n;
}

inline uint32_t get2DIndex(uint32_t n, uint32_t row, uint32_t col) {
  return row * n + col;
}

#endif

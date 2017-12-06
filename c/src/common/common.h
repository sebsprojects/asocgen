#ifndef COMMON
#define COMMON

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef int32_t bool;


// --------------------------------------------------------------------------
// String and Printing
// --------------------------------------------------------------------------

void padString(char *string, uint32_t pad);
void padStringForInt(char *string, uint32_t val, uint32_t numDigits);
void sprint_uint(char *string, uint32_t val);

void printError(char *error);
void sprintBinary_uint8(char *pstring, uint8_t n);
void sprintBinary_uint64(char *pstring, uint64_t n);


// --------------------------------------------------------------------------
// Math
// --------------------------------------------------------------------------

uint32_t factorial(uint32_t n);

struct Array_uint16;
struct Array_uint16 *getPrimeFactors_alloc(uint16_t n);
struct Array_uint16 *getFactors_alloc(uint16_t n);


// --------------------------------------------------------------------------
// Inline Functions
// --------------------------------------------------------------------------

inline uint8_t getRowIndex(uint32_t n, uint32_t ind) {
  return ind / n;
}

inline uint8_t getColIndex(uint32_t n, uint32_t ind) {
  return ind % n;
}

inline uint32_t get2DIndex(uint32_t n, uint32_t row, uint32_t col) {
  return row * n + col;
}

inline uint32_t umin(uint32_t a, uint32_t b) {
  return a < b ? a : b;
}

inline uint32_t umax(uint32_t a, uint32_t b) {
  return a < b ? b : a;
}

#endif

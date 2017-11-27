#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "common.h"
#include "sarray.h"
#include "sbitfield.h"
#include "smap.h"


void padString(char *string, uint32_t val) {
  if(val < 10) sprintf(string + strlen(string), "  ");
  else if(val < 100) sprintf(string + strlen(string), " ");
}

void sprint_uint(char *string, uint32_t val) {
  if(val == 0xff) {
    padString(string, 0);
    sprintf(string + strlen(string), "~ ");
  } else {
    padString(string, val);
    sprintf(string + strlen(string), "%u ", val);
  }
}

void sprintArray_uint8(char *string, Array_uint8 *arr) {
  uint32_t n = arr->size;
  uint32_t i;
  string[0] = '\0';

  sprintf(string, "[ ");
  for(i = 0; i < n; i++) {
    padString(string, *at_uint8(arr, i));
    sprintf(string + strlen(string), "%u", *at_uint8(arr, i));
    if(i < n - 1) sprintf(string + strlen(string), ", ");
    if((i + 1) % 10 == 0) sprintf(string + strlen(string), "\n  ");
  }
  sprintf(string + strlen(string), " ]\n");
}

void sprintArray_uint16(char *string, Array_uint16 *arr) {
  uint32_t n = arr->size;
  uint32_t i;
  string[0] = '\0';

  sprintf(string, "[ ");
  for(i = 0; i < n; i++) {
    padString(string, *at_uint16(arr, i));
    sprintf(string + strlen(string), "%u", *at_uint16(arr, i));
    if(i < n - 1) sprintf(string + strlen(string), ", ");
    if((i + 1) % 10 == 0) sprintf(string + strlen(string), "\n  ");
  }
  sprintf(string + strlen(string), " ]\n");
}

void sprintArraySquare_uint8(char *string, Array_uint8 *arr, uint32_t sq) {
  uint32_t i, j;
  uint8_t ele;
  string[0] = '\0';

  for(i = 0; i < sq; i++) {
    for(j = 0; j < sq; j++) {
      ele = *at_uint8(arr, i * sq + j);
      sprint_uint(string, ele);
    }
    sprintf(string + strlen(string), "\n");
  }
}

void sprintArraySquare_uint16(char *string, Array_uint16 *arr, uint32_t sq) {
  uint32_t i, j;
  uint8_t ele;
  string[0] = '\0';

  for(i = 0; i < sq; i++) {
    for(j = 0; j < sq; j++) {
      ele = *at_uint16(arr, i * sq + j);
      sprint_uint(string, ele);
    }
    sprintf(string + strlen(string), "\n");
  }
}

void printArray_uint8(char *string, Array_uint8 *arr) {
  sprintArray_uint8(string, arr);
  printf(string);
}

void printArray_uint16(char *string, Array_uint16 *arr) {
  sprintArray_uint16(string, arr);
  printf(string);
}

void printError(char *error) {
  fprintf(stderr, "%s", error);
}

void sprintBinary_uint8(char *pstring, uint8_t n) {
  uint32_t i;
  for(i = 0; i < 16; i++) {
    if((n & (1 << (15 - i))) == 0) {
      pstring[i] = '0';
    } else {
      pstring[i] = '1';
    }
  }
}

void sprintBitfield(char *pstring, Bitfield *bf) {
  pstring[bf->bitCount] = '\0';
  uint32_t i;
  for(i = 0; i < bf->bitCount; i++) {
    if(testBit(bf, i) == 0) {
      pstring[bf->bitCount - i - 1] = '0';
    } else {
      pstring[bf->bitCount - i - 1] = '1';
    }
  }
}

void printBitfield(char *pstring, Bitfield *bf) {
  sprintBitfield(pstring, bf);
  printf("%s\n", pstring);
}

uint32_t factorial(uint32_t n) {
#ifdef BOUNDS_CHECK
  if(n > 12) {
    printError("error: factorial only fits into uint32 for n <= 12");
    exit(1);
  }
#endif
  if(n == 0) {
    return 1;
  } else {
    return n * factorial(n - 1);
  }
}

#include <string.h>
#include <math.h>

#include "common.h"
#include "sarray16.h"
#include "sbitfield.h"
#include "smap.h"


void padStringForInt(char *string, uint32_t val) {
  if(val < 10) sprintf(string + strlen(string), "  ");
  else if(val < 100) sprintf(string + strlen(string), " ");
}

void sprint_uint(char *string, uint32_t val) {
  if(val == 0xff) {
    padStringForInt(string, 0);
    sprintf(string + strlen(string), "~ ");
  } else {
    padStringForInt(string, val);
    sprintf(string + strlen(string), "%u ", val);
  }
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

// --------------------------------------------------------------------------

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

Array_uint16 *getPrimeFactors_alloc(uint16_t n) {
  Array_uint16 *factors = aui16_alloc(n / 2); // *might* be improved
  uint32_t i;
  uint32_t c = 0;
  uint32_t sqrtn = floor(sqrt(n));
  for(i = 2; i <= sqrtn; i++) {
    while(n % i == 0) {
      *aui16_at(factors, c) = i;
      c++;
      n = n / i;
    }
  }
  if(n > 2) {
    *aui16_at(factors, c) = n;
    c++;
  }
  aui16_shrink(factors, c);
  return factors;
}

Array_uint16 *getFactors_alloc(uint16_t n) {
  Array_uint16 *factors = aui16_alloc(n / 2); // *might* be improved
  uint32_t i;
  uint32_t c = 0;
  for(i = 1; i <= n; i++) {
    if(n % i == 0) {
      *aui16_at(factors, c) = i;
      c++;
    }
  }
  aui16_shrink(factors, c);
  return factors;
}

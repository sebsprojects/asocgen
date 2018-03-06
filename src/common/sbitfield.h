#ifndef SBITFIELD
#define SBITFIELD

#include <stdio.h>
#include "sarray64.h"

/*
  In a Bitfield individual bits can be maniuplated. This can be used for
  storage-efficient masks.

  The implementation is based on Array_uint64.

  The functions
    bitfield_trailingZeros
    bitfield_sumBits
    bitfield_leadingZeros
  are dependet on GCC built-ins and currently only available with GCC
 */

struct Bitfield {
  uint32_t bitCount;
  Array_uint64 *array;
};
typedef struct Bitfield Bitfield;


// --------------------------------------------------------------------------
// Alloc / Free
// --------------------------------------------------------------------------

Bitfield *bitfield_alloc(uint32_t bitCount);
void bitfield_free(Bitfield *bf);


// --------------------------------------------------------------------------
// Printing
// --------------------------------------------------------------------------

void bitfield_sprint(char *pstring, struct Bitfield *bf);
void bitfield_print(char *pstring, struct Bitfield *bf);


// --------------------------------------------------------------------------
// Inline Functions
// --------------------------------------------------------------------------

inline void bitfield_clear(Bitfield *bf) {
  uint32_t i;
  for(i = 0; i < bf->array->size; i++) {
    *aui64_at(bf->array, i) = 0;
  }
}

inline void bitfield_setBit(Bitfield *bf, const uint32_t index) {
#ifdef BOUNDS_CHECK
  if(index >= bf->bitCount) {
    fprintf(stderr,
            "error: Bitfield setBit out of bounds: size=%u, index=%u \n",
	    bf->bitCount, index);
    exit(1);
  }
#endif
  *aui64_at(bf->array, index / 64) |= (1 << (index % 64));
}

inline void bitfield_unsetBit(Bitfield *bf, const uint32_t index) {
#ifdef BOUNDS_CHECK
  if(index >= bf->bitCount) {
    fprintf(stderr,
            "error: Bitfield unsetBit out of bounds: size=%u, index=%u \n",
	    bf->bitCount, index);
    exit(1);
  }
#endif
  *aui64_at(bf->array, index / 64) &= ~(1 << (index % 64));
}

inline void bitfield_toggleBit(Bitfield *bf, const uint32_t index) {
#ifdef BOUNDS_CHECK
  if(index >= bf->bitCount) {
    fprintf(stderr,
            "error: Bitfield toogleBit out of bounds: size=%u, index=%u \n",
	    bf->bitCount, index);
    exit(1);
  }
#endif
  *aui64_at(bf->array, index / 64) ^= (1 << (index % 64));
}

inline void bitfield_placeBit(Bitfield *bf, const uint32_t index,
                              const uint32_t b) {
#ifdef BOUNDS_CHECK
  if(index >= bf->bitCount) {
    fprintf(stderr,
            "error: Bitfield toogleBit out of bounds: size=%u, index=%u \n",
	    bf->bitCount, index);
    exit(1);
  }
  if(b != 0 && b != 1) {
    fprintf(stderr, "error: Bitfield placeBit bit is not 0 or 1 but %u\n",
            b);
    exit(1);
  }
#endif
  if(b == 0) {
    bitfield_unsetBit(bf, index);
  } else {
    bitfield_setBit(bf, index);
  }
}

inline uint32_t bitfield_testBit(Bitfield *bf, const uint32_t index) {
#ifdef BOUNDS_CHECK
  if(index >= bf->bitCount) {
    fprintf(stderr,
            "error: Bitfield toogleBit out of bounds: size=%u, index=%u \n",
	    bf->bitCount, index);
    exit(1);
  }
#endif
  return ((*aui64_at(bf->array, index / 64)) >> (index % 64)) & 1;
}

#ifdef __GNUC__ // ---------------------------------------------------------

inline uint32_t bitfield_sumBits(Bitfield *bf) {
  uint32_t sum = 0;
  uint32_t i;
  for(i = 0; i < bf->array->size; i++) {
    sum += __builtin_popcountll(*aui64_at(bf->array, i));
  }
  return sum;
}

inline uint32_t bitfield_trailingZeros(Bitfield *bf) {
  uint32_t i;
  uint64_t ele;
  for(i = 0; i < bf->array->size; i++) {
    ele = *aui64_at(bf->array, i);
    if(ele == 0) {
      continue;
    }
    return i * 64 + __builtin_ctzll(ele);
  }
}

inline uint32_t bitfield_leadingZeros(Bitfield *bf) {
  int32_t i;
  uint64_t ele;
  for(i = bf->array->size - 1; i >= 0; i--) {
    ele = *aui64_at(bf->array, i);
    if(ele == 0) {
      continue;
    }
    return i * 64 + __builtin_clzll(ele) - (64 - bf->bitCount);
  }
}

#else // -------------------------------------------------------------------

inline uint32_t sumBits(Bitfield *bf) {
  fprintf(stderr, "error: Bitfield sumBits is not implemented yet.\n");
  exit(1);
}

inline uint32_t trailingZeros(Bitfield *bf) {
  fprintf(stderr, "error: Bitfield trailingZeros is not implemented yet.\n");
  exit(1);
}

inline uint32_t leadingZeros(Bitfield *bf) {
  fprintf(stderr, "error: Bitfield leadingZeros is not implemented yet.\n");
  exit(1);
}

#endif // ------------------------------------------------------------------

#endif

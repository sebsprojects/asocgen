#ifndef SBITFIELD
#define SBITFIELD

#include "sarray.h"


struct Bitfield {
  uint32_t bitCount;
  Array_uint64 *array;
};
typedef struct Bitfield Bitfield;

Bitfield *allocBitfield(uint32_t bitCount);
void freeBitfield(Bitfield *bf);

inline void clearBitfield(Bitfield *bf) {
  uint32_t i;
  for(i = 0; i < bf->array->size; i++) {
    *at_uint64(bf->array, i) = 0;
  }
}

inline void setBit(Bitfield *bf, const uint32_t index) {
#ifdef BOUNDS_CHECK
  if(index >= bf->bitCount) {
    fprintf(stderr,
            "error: Bitfield setBit out of bounds: size=%u, index=%u \n",
	    bf->bitCount, index);
    exit(1);
  }
#endif
  *at_uint64(bf->array, index / 64) |= (1 << (index % 64));
}

inline void unsetBit(Bitfield *bf, const uint32_t index) {
#ifdef BOUNDS_CHECK
  if(index >= bf->bitCount) {
    fprintf(stderr,
            "error: Bitfield unsetBit out of bounds: size=%u, index=%u \n",
	    bf->bitCount, index);
    exit(1);
  }
#endif
  *at_uint64(bf->array, index / 64) &= ~(1 << (index % 64));
}

inline void toggleBit(Bitfield *bf, const uint32_t index) {
#ifdef BOUNDS_CHECK
  if(index >= bf->bitCount) {
    fprintf(stderr,
            "error: Bitfield toogleBit out of bounds: size=%u, index=%u \n",
	    bf->bitCount, index);
    exit(1);
  }
#endif
  *at_uint64(bf->array, index / 64) ^= (1 << (index % 64));
}

inline void placeBit(Bitfield *bf, const uint32_t index, const uint32_t b) {
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
  if(b == 0) unsetBit(bf, index);
  else setBit(bf, index);
}

inline uint32_t testBit(Bitfield *bf, const uint32_t index) {
#ifdef BOUNDS_CHECK
  if(index >= bf->bitCount) {
    fprintf(stderr,
            "error: Bitfield toogleBit out of bounds: size=%u, index=%u \n",
	    bf->bitCount, index);
    exit(1);
  }
#endif
  return ((*at_uint64(bf->array, index / 64)) >> (index % 64)) & 1;
}

inline uint32_t sumBits(Bitfield *bf) {
  uint32_t sum = 0;
  uint32_t i;
  for(i = 0; i < bf->array->size; i++) {
    sum += __builtin_popcountll(*at_uint64(bf->array, i));
  }
  return sum;
}

inline uint32_t trailingZeros(Bitfield *bf) {
  uint32_t i;
  uint64_t ele;
  for(i = 0; i < bf->array->size; i++) {
    ele = *at_uint64(bf->array, i);
    if(ele == 0) continue;
    return i * 64 + __builtin_ctzll(ele);
  }
}

inline uint32_t leadingZeros(Bitfield *bf) {
  int32_t i;
  uint64_t ele;
  for(i = bf->array->size - 1; i >= 0; i--) {
    ele = *at_uint64(bf->array, i);
    if(ele == 0) continue;
    return i * 64 + __builtin_clzll(ele) - (64 - bf->bitCount);
  }
}

#endif

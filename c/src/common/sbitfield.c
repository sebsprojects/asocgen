#include "sbitfield.h"


Bitfield *allocBitfield(uint32_t bitCount) {
  Bitfield *bf = malloc(sizeof(Bitfield));
  bf->bitCount = bitCount;
  bf->array = allocArray_uint64(1 + bitCount / 64);
  return bf;
}

void freeBitfield(Bitfield *bf) {
  freeArray_uint64(bf->array);
  free(bf);
}

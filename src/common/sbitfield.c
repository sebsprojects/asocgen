#include "sbitfield.h"

Bitfield *bitfield_alloc(uint32_t bitCount) {
  Bitfield *bf = malloc(sizeof(Bitfield));
  bf->bitCount = bitCount;
  bf->array = aui64_alloc(1 + bitCount / 64);
  return bf;
}

void bitfield_free(Bitfield *bf) {
  aui64_free(bf->array);
  free(bf);
}

// --------------------------------------------------------------------------

void bitfield_sprint(char *pstring, Bitfield *bf) {
  pstring[bf->bitCount] = '\0';
  uint32_t i;
  for(i = 0; i < bf->bitCount; i++) {
    if(bitfield_testBit(bf, i) == 0) {
      pstring[bf->bitCount - i - 1] = '0';
    } else {
      pstring[bf->bitCount - i - 1] = '1';
    }
  }
}

void bitfield_print(char *pstring, Bitfield *bf) {
  bitfield_sprint(pstring, bf);
  printf("%s\n", pstring);
}

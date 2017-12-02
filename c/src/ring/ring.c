#include "ring.h"


Ring *ring_alloc(uint16_t order, bool indexed) {
  Ring *ring = malloc(sizeof(Ring));
  ring->set = allocArray_uint16(order);
  ring->mtab = allocArray_uint16(order * order);
  ring->atab = allocArray_uint16(order * order);
  ring->indexed = indexed;
  return ring;
}

void ring_free(Ring *ring) {
  freeArray_uint16(ring->atab);
  freeArray_uint16(ring->mtab);
  freeArray_uint16(ring->set);
  free(ring);
}

bool ring_isCommutative(Ring *ring) {
  uint32_t i, j;
  uint16_t n = ring_order(ring);
  for(i = 0; i < n; i++) {
    for(j = 0; j < n; j++) {
      if(mopi(ring, i, j) != mopi(ring, j, i)) return 0;
    }
  }
  return 1;
}

uint16_t ring_zero(Ring *ring) {
  return *at_uint16(ring->set, ring_zeroi(ring));
}

// Assumes additive group is valid (has unique zero element)
uint32_t ring_zeroi(Ring *ring) {
  uint32_t i;
  for(i = 0; i < ring_order(ring); i++) {
    if(aopi(ring, i, 0) == 0) {
      return i;
    }
  }
  return 0xffffffff;
}

uint16_t ring_one(Ring *ring) {
  return *at_uint16(ring->set, ring_onei(ring));
}

uint32_t ring_onei(Ring *ring) {
  uint32_t i, j;
  uint16_t n = ring_order(ring);
  bool isOne = 1;
  for(i = 0; i < n; i++) {
    for(j = 0; j < n; j++) {
      isOne = isOne && mopi(ring, i, j) == j && mopi(ring, j, i) == j;
    }
    if(isOne) {
      return i;
    }
    isOne = 1;
  }
  return 0xffffffff;
}

bool ring_hasOne(Ring *ring) {
  return ring_onei(ring) < ring_order(ring);
}

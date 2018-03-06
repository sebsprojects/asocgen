#include "ring_common.h"

#include "../common/common_includes.h"


Ring *createZnZ(uint32_t n) {
  Ring *ring = ring_alloc(n, 1);
  aui16_setToRange(ring->set, 0, n, 0);
  uint32_t i, j, ind;
  for(i = 0; i < n; i++) {
    for(j = 0; j < n; j++) {
      ind = get2DIndex(n, i, j);
      *aui16_at(ring->atab, ind) = (i + j) % n;
      *aui16_at(ring->mtab, ind) = (i * j) % n;
    }
  }
  return ring;
}

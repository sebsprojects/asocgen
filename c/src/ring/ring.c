#include "ring.h"

Ring *ring_alloc(uint16_t order, bool indexed) {
  Ring *ring = malloc(sizeof(Ring));
  ring->set = aui16_alloc(order);
  ring->mtab = aui16_alloc(order * order);
  ring->atab = aui16_alloc(order * order);
  ring->indexed = indexed;
  return ring;
}

void ring_free(Ring *ring) {
  aui16_free(ring->atab);
  aui16_free(ring->mtab);
  aui16_free(ring->set);
  free(ring);
}

Group *ring_getAdditiveGroup_alloc(Ring *ring) {
  Group *group = group_alloc(ring->set->size, ring->indexed);
  aui16_copyInto(group->set, ring->set);
  aui16_copyInto(group->gtab, ring->atab);
  return group;
}

Group *ring_getAdditiveGroup_ref(Ring *ring) {
  Group *group = malloc(sizeof(Group));
  group->indexed = ring->indexed;
  group->set = ring->set;
  group->gtab = ring->atab;
  return group;
}

void ring_freeAdditiveGroup_ref(Group *group) {
  free(group);
}

Group *ring_getUnitGroup_alloc(Ring *ring) {
  Array_uint16 *unitis = aui16_alloc(ring->set->size);
  uint32_t i, j;
  uint16_t count = 0;
  for(i = 0; i < ring_order(ring); i++) {
    if(ring_isUniti(ring, i)) {
      *aui16_at(unitis, count) = i; // store the index of the unit in ring
      count++;
    }
  }
  aui16_shrink(unitis, count);
  Group *group = group_alloc(unitis->size, 0);
  uint16_t ind1, ind2;
  for(i = 0; i < unitis->size; i++) {
    ind1 = *aui16_at(unitis, i); // first unit index
    *aui16_at(group->set, i) = *aui16_at(ring->set, ind1); // set the ele
    for(j = 0; j <= i; j++) {
      ind2 = *aui16_at(unitis, j); // second unit index
      *aui16_at(group->gtab, get2DIndex(unitis->size, i, j)) =
        *aui16_at(ring->set, mopi(ring, ind1, ind2)); // set the ele
      if(i != j) {
        *aui16_at(group->gtab, get2DIndex(unitis->size, j, i)) =
          *aui16_at(ring->set, mopi(ring, ind2, ind1)); // set the ele
      }
    }
  }
  aui16_free(unitis);
  return group;
}

// --------------------------------------------------------------------------

uint16_t ring_zero(Ring *ring) {
  return *aui16_at(ring->set, ring_zeroi(ring));
}

// Assumes additive group is valid (has unique zero element)
uint16_t ring_zeroi(Ring *ring) {
  uint32_t i;
  for(i = 0; i < ring_order(ring); i++) {
    if(aopi(ring, i, 0) == 0) {
      return i;
    }
  }
  return 0xffff;
}

uint16_t ring_one(Ring *ring) {
  return *aui16_at(ring->set, ring_onei(ring));
}

uint16_t ring_onei(Ring *ring) {
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
  return 0xffff;
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

bool ring_isUniti(Ring *ring, uint16_t ind) {
  uint16_t oneInd = ring_onei(ring);
  uint32_t i;
  for(i = 0; i < ring_order(ring); i++) {
    if(mopi(ring, i, ind) == oneInd && mopi(ring, ind, i) == oneInd) {
      return 1;
    }
  }
  return 0;
}

bool ring_isUnit(Ring *ring, uint16_t ele) {
  return ring_isUniti(ring, aui16_indexOf(ring->set, ele));
}

bool ring_hasZeroDivisors(Ring *ring) {
  uint32_t i, j;
  uint32_t n = ring_order(ring);
  uint16_t zeroInd = ring_zeroi(ring);
  for(i = 0; i < n; i++) {
    for(j = 0; j <= i; j++) {
      if(mopi(ring, i, j) == zeroInd || mopi(ring, j, i) == zeroInd) {
        return 1;
      }
    }
  }
  return 0;
}

bool ring_isIntegralDomain(Ring *ring) {
  return !ring_hasZeroDivisors(ring) && ring_isCommutative(ring);
}

bool ring_isField(Ring *ring) {
  uint32_t i;
  uint16_t zeroInd = ring_zeroi(ring);
  for(i = 0; i < ring_order(ring); i++) {
    if(i != zeroInd && !ring_isUniti(ring, i)) {
      return 0;
    }
  }
  return ring_isCommutative(ring);
}

// --------------------------------------------------------------------------

bool ring_isValid(Ring *ring) {
  return ring_hasOne(ring) &&
    ring_addIsCommutativeGroup(ring) &&
    ring_mulIsAssociative(ring) &&
    ring_isDistributive(ring);
}

bool ring_hasOne(Ring *ring) {
  return ring_onei(ring) < ring_order(ring);
}

bool ring_addIsCommutativeGroup(Ring *ring) {
  Group *group = ring_getAdditiveGroup_ref(ring);
  bool isAddGroup = group_isValid(group) && group_isCommutative(group);
  ring_freeAdditiveGroup_ref(group);
  return isAddGroup;
}

bool ring_mulIsAssociative(Ring *ring) {
  uint16_t a, b, c, ab, ab_c, bc, a_bc;
  uint32_t n = ring_order(ring);
  for(a = 0; a < n; a++) {
    for(b = 0; b < n; b++) {
      for(c = 0; c < n; c++) {
	ab = mopi(ring, a, b);
	bc = mopi(ring, b, c);
        ab_c = mopi(ring, ab, c);
	a_bc = mopi(ring, a, bc);
	if(ab_c != a_bc) return 0;
      }
    }
  }
  return 1;
}

// check a(b+c) = ab + ac
//       (b+c)a = ba + ca
bool ring_isDistributive(Ring *ring) {
  uint16_t a, b, c, bPlusC, prod, sum1, sum2;
  uint32_t n = ring_order(ring);
  for(b= 0; b < n; b++) {
    for(c = 0; c < n; c++) {
      bPlusC = aopi(ring, b, c);
      for(a = 0; a < n; a++) {
        // first case
        prod = mopi(ring, a, bPlusC);
        sum1 = mopi(ring, a, b);
        sum2 = mopi(ring, a, c);
        if(prod != aopi(ring, sum1, sum2)) {
          return 0;
        }
        // second case
        prod = mopi(ring, bPlusC, a);
        sum1 = mopi(ring, b, a);
        sum2 = mopi(ring, c, a);
        if(prod != aopi(ring, sum1, sum2)) {
          return 0;
        }
      }
    }
  }
  return 1;
}

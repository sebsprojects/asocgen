#ifndef RING
#define RING

#include "../common/common_includes.h"
#include "../group/group.h"

/*
  Represents a Ring. If indexed is 1, then set must be id = { 0, 1, ..., n }
  A valid ring must have a multiplicative identity (1-element).
  Ring homomorphisms must map multiplicative identities.
 */

struct Ring {
  bool indexed;
  Array_uint16* set;
  Array_uint16* mtab;
  Array_uint16* atab;
};
typedef struct Ring Ring;


// --------------------------------------------------------------------------
// Management
// --------------------------------------------------------------------------

Ring *ring_alloc(uint16_t order, bool indexed);
void ring_free(Ring *ring);

Group *ring_getAdditiveGroup_alloc(Ring *ring); // free like a normal group
Group *ring_getAdditiveGroup_ref(Ring *ring); //use ring_freeAdditiveGroup_ref
void ring_freeAdditiveGroup_ref(Group *group);

Group *ring_getUnitGroup_alloc(Ring *ring);


// --------------------------------------------------------------------------
// Information
// --------------------------------------------------------------------------

uint16_t ring_zero(Ring *ring);
uint16_t ring_zeroi(Ring *ring);
uint16_t ring_one(Ring *ring);
uint16_t ring_onei(Ring *ring);

bool ring_isCommutative(Ring *ring);

bool ring_isUniti(Ring *ring, uint16_t ind);
bool ring_isUnit(Ring *ring, uint16_t ele);

bool ring_isZeroDivisori(Ring *ring, uint16_t ind);
bool ring_isZeroDivisor(Ring *ring, uint16_t ele);

bool ring_hasZeroDivisors(Ring *ring);
bool ring_isIntegralDomain(Ring *ring);
bool ring_isField(Ring *ring);


// --------------------------------------------------------------------------
// Validation
// --------------------------------------------------------------------------

bool ring_isValid(Ring *ring);
bool ring_hasOne(Ring *ring);
bool ring_addIsCommutativeGroup(Ring *ring);
bool ring_mulIsAssociative(Ring *ring);
bool ring_isDistributive(Ring *ring);


// --------------------------------------------------------------------------
// Inline Functions
// --------------------------------------------------------------------------

inline uint16_t ring_order(Ring *ring) {
  return ring->set->size;
}

inline uint16_t aop(Ring *ring, uint16_t i, uint16_t j) {
    if(!ring->indexed) {
    i = aui16_indexOf(ring->set, i);
    j = aui16_indexOf(ring->set, j);
  }
  return *aui16_at(ring->atab, get2DIndex(ring_order(ring), i, j));
}

inline uint16_t aopi(Ring *ring, uint16_t i, uint16_t j) {
  uint16_t ele = *aui16_at(ring->atab, get2DIndex(ring_order(ring), i, j));
  if(!ring->indexed) {
    return aui16_indexOf(ring->set, ele);
  }
  return ele;
}

inline uint16_t mop(Ring *ring, uint16_t i, uint16_t j) {
  if(!ring->indexed) {
    i = aui16_indexOf(ring->set, i);
    j = aui16_indexOf(ring->set, j);
  }
  return *aui16_at(ring->mtab, get2DIndex(ring_order(ring), i, j));
}

inline uint16_t mopi(Ring *ring, uint16_t i, uint16_t j) {
  uint16_t ele = *aui16_at(ring->mtab, get2DIndex(ring_order(ring), i, j));
  if(!ring->indexed) {
    return aui16_indexOf(ring->set, ele);
  }
  return ele;
}

#endif

#ifndef GROUP
#define GROUP

#include "../common/common_includes.h"


/* Requirements:
     if indexed == 1: set = {0, 1, ..., n}
     else           : set = {0 < e1 < e2 < .. < en}
 */
struct Group {
  bool indexed;
  Array_uint16 *set;
  Array_uint16 *gtab;
  Array_uint16 *invs;
};
typedef struct Group Group;


bool group_isCommutative(Group *group);
bool group_isCyclic(Group *group);
uint16_t group_elementOrder(Group *group, uint16_t ele);
uint16_t group_elementOrderi(Group *group, uint16_t ind);

uint16_t group_neutral(Group *group);
uint32_t group_neutrali(Group *group);

Group *group_alloc(uint16_t order, bool indexed);

void group_free(Group *group);
void group_setInvs(Group *group);
bool group_isValid(Group *group);
bool group_hasValidOp(Group *group);
bool group_isAsoc(Group *group);
bool group_hasNeutral(Group *group);
bool group_hasInvs(Group *group);

Map_uint16 *getOrderDistribution(Group *group);

void group_printSummary(Group *group);
void group_print(Group *group);

/*
 *
 * INLINE IMPLEMENTATION
 *
 */

inline uint16_t group_order(Group *group) {
  return group->set->size;
}

inline uint16_t group_inv(Group *group, uint16_t ele) {
  uint16_t ind = ele;
  if(!group->indexed) {
    ind = indexof_uint16(group->set, ele);
  }
  return *at_uint16(group->invs, ind);
}

inline uint16_t group_invi(Group *group, uint16_t ind) {
  return *at_uint16(group->invs, ind);
}

// ele * ele -> ele
inline uint16_t gop(Group *group, uint16_t i, uint16_t j) {
  if(!group->indexed) {
    i = indexof_uint16(group->set, i);
    j = indexof_uint16(group->set, j);
  }
  return *at_uint16(group->gtab, get2DIndex(group_order(group), i, j));
}

// index * index -> index
inline uint16_t gopi(Group *group, uint16_t i, uint16_t j) {
  uint16_t ele = *at_uint16(group->gtab, get2DIndex(group_order(group), i, j));
  if(!group->indexed) {
    return indexof_uint16(group->set, ele);
  }
  return ele;
}

#endif

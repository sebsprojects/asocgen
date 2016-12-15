#ifndef GROUP
#define GROUP

#include "../common/common.h"


/* Requirements:
     if indexed == 1: set = {0, 1, ..., n}
     else           : set = {0 < e1 < e2 < .. < en}
 */
struct Group {
  bool indexed;
  Array_uint16 *set;
  Array_uint16 *mtab;
  Array_uint16 *invs;
};
typedef struct Group Group;


/* Action functions */
// uint16_t gop(Group *group, uint16_t i, uint16_t j);
// uint16_t gopi(Group *group, uint16_t i, uint16_t j);
// uint16_t inv(Group *group, uint16_t ele);
// uint16_t invi(Group *group, uint16_t ind);

/* Info functions */
// uint16_t order(Group *group);
bool isCommutative(Group *group);
bool isCyclic(Group *group);
uint16_t elementOrder(Group *group, uint16_t ele);
uint16_t elementOrderi(Group *group, uint16_t ind);

/* Managing functions */
Group *allocGroup(uint16_t order);
void freeGroup(Group *group);
void setInvs(Group *group);
bool isValid(Group *group);
bool isAsoc(Group *group);
bool hasNeutral(Group *group);
bool hasInvs(Group *group);
void printGroup(Group *group);


/*
 *
 * INLINE IMPLEMENTATION
 *
 */


inline uint16_t order(Group *group) {
  return group->set->size;
}

inline uint16_t inv(Group *group, uint16_t ele) {
  if(!group->indexed) {
    ele = indexof_uint16(group->set, ele);
  }
  return *at_uint16(group->invs, ele);
}

inline uint16_t invi(Group *group, uint16_t ind) {
  return *at_uint16(group->invs, ind);
}

// index * index -> index
inline uint16_t gopi(Group *group, uint16_t i, uint16_t j) {
  uint16_t ele = *at_uint16(group->mtab, get2DIndex(order(group), i, j));
  if(!group->indexed) {
    return indexof_uint16(group->set, ele);
  }
  return ele;
}

// ele * ele -> ele
inline uint16_t gop(Group *group, uint16_t i, uint16_t j) {
  if(!group->indexed) {
    i = indexof_uint16(group->set, i);
    j = indexof_uint16(group->set, j);
  }
  return *at_uint16(group->mtab, get2DIndex(order(group), i, j));
}

#endif

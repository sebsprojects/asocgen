#ifndef GROUP
#define GROUP

#include "../common/common.h"


/*
  Groups of the following form:
     set:  { 0, ... , n } in this order
     invs: { inv(0), ... , inv(n) } in this order
     mtab: Multiplication table using the order of set

     0 is always the neutral element
 */

struct Group {
  Array_uint16 *set;
  Array_uint16 *invs;
  Array_uint16 *mtab;
};
typedef struct Group Group;

bool isCommutative(Group *group);
bool isCyclic(Group *group);
uint16_t elementOrder(Group *group, uint16_t ele);

Array_uint16 generateFrom(Group *group, Array_uint16 set);
Array_uint16 minimalGeneratingSet(Group *group);

uint16_t compInv(Group *group, uint16_t ele);

inline uint16_t order(Group *group) {
  return group->set->size;
}

inline uint16_t inv(Group *group, uint16_t ele) {
  return *at_uint16(group->invs, ele);
}

inline uint16_t gop(Group *group, uint16_t a, uint16_t b) {
  return *at_uint16(group->mtab, get2DIndex(order(group), a, b));
}

Group *allocGroup(uint16_t order);
void freeGroup(Group *group);

bool isValid(Group *group);
bool isAsoc(Group *group);
bool hasNeutral(Group *group);
bool hasInvs(Group *group);

void printGroup(Group *group);

#endif

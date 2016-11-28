#ifndef GROUP
#define GROUP

#include "../common/common.h"


struct Group {
  bool indexed;
  Array_uint16 *set;
  Array_uint16 *mtab;
  Array_uint16 *invs;
  Map_uint16 *imap;
};
typedef struct Group Group;


bool isCommutative(Group *group);
bool isCyclic(Group *group);
uint16_t elementOrder(Group *group, uint16_t ele);
uint16_t compInv(Group *group, uint16_t ele);

void setInvs(Group *group);

inline uint16_t order(Group *group) {
  return group->set->size;
}

inline uint16_t inv(Group *group, uint16_t ele) {
  if(!group->indexed) {
    ele = mapFrom_uint16(group->imap, ele);
  }
  return *at_uint16(group->invs, ele);
}

// index * index -> index
inline uint16_t gopi(Group *group, uint16_t i, uint16_t j) {
  uint16_t ele = *at_uint16(group->mtab, get2DIndex(order(group), i, j));
  if(!group->indexed) {
    return mapFrom_uint16(group->imap, ele);
  }
  return ele;
}

// ele * ele -> ele
inline uint16_t gop(Group *group, uint16_t i, uint16_t j) {
  if(!group->indexed) {
    i = mapFrom_uint16(group->imap, i);
    j = mapFrom_uint16(group->imap, j);
  }
  return *at_uint16(group->mtab, get2DIndex(order(group), i, j));
}

Group *allocGroup(uint16_t order);
void freeGroup(Group *group);

bool isValid(Group *group);
bool isAsoc(Group *group);
bool hasNeutral(Group *group);
bool hasInvs(Group *group);

void printGroup(Group *group);

#endif

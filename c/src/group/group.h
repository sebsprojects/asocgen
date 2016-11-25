#ifndef GROUP
#define GROUP

#include "../common/common.h"


struct Group {
  Array_uint16 *set;
  Array_uint16 *invs;
  Array_uint16 *mtab;
};
typedef struct Group Group;

Group *allocGroup(uint16_t order);
void freeGroup(Group *group);

uint16_t order(Group *group);

bool isValid(Group *group);
bool isAsoc(Group *group);
bool hasNeutral(Group *group);
bool hasInvs(Group *group);

void printGroup(Group *group);

#endif

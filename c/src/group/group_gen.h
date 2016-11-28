#ifndef GROUP_GEN
#define GROUP_GEN

#include "group.h"
#include "../common/common.h"


void generateFrom_noalloc(Group *group, Array_uint16 *set, Array_uint16 *res);
Array_uint16 *generateFrom_toset(Group *group, Array_uint16 *set);
Group *generateFrom_togroup(Group *group, Array_uint16 *set);

void truncGeneratedSet(Group *group, Array_uint16 *res);

Array_uint16 *minimalGeneratingSet(Group *group);


#endif

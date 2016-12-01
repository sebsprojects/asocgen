#include "subgroup.h"
#include "group_gen.h"


Group *generateSubgroup(Group *group, Array_uint16 *set) {
  Array_uint16 *res = generateFrom_alloc(group, set);
  truncGeneratedSet(group, res, 1);
  uint32_t m = res->size;    // Subgroup order
  Group *subgroup = allocGroup(m);
  subgroup->indexed = 0;
  uint32_t i, j;
  uint16_t a, b;
  for(i = 0; i < m; i++) {
    a = *at_uint16(res, i);
    *at_uint16(subgroup->set, i) = a;
  }
  for(i = 0; i < m; i++) {
    a = *at_uint16(res, i);
    for(j = 0; j <= i; j++) {
      b = *at_uint16(res, j);
      *at_uint16(subgroup->mtab, get2DIndex(m, i, j)) =
        gop(group, a, b);
      *at_uint16(subgroup->mtab, get2DIndex(m, j, i)) =
        gop(group, b, a);
    }
  }
  setInvs(subgroup);
  freeArray_uint16(res);
  return subgroup;
}

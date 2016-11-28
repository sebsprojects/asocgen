#include "group_common.h"

Group *createCn(uint32_t n) {
  Group *group = allocGroup(n);
  uint32_t i, j;
  for(i = 0; i < n; i++) {
    *at_uint16(group->set, i) = i;
    for(j = 0; j <= i; j++) {
      *at_uint16(group->mtab, get2DIndex(n, i, j)) = (i + j) % n;
      *at_uint16(group->mtab, get2DIndex(n, j, i)) = (j + i) % n;
    }
    *at_uint16(group->invs, i) = (n - i) % n;
  }
  return group;
}

Group *createFromGen(uint32_t n, Array_uint8 *mtab) {
  Group *group = allocGroup(n);
  uint32_t i;
  for(i = 0; i < n; i++) {
    *at_uint16(group->set, i) = i;
  }
  for(i = 0; i < n * n; i++) {
    *at_uint16(group->mtab, i) = *at_uint8(mtab, i);
  }
  setInvs(group);
  return group;
}

#include "group_common.h"

Group *createCn(uint32_t n) {
  Group *group = allocGroup(n);
  uint32_t i, j;
  for(i = 0; i < n; i++) {
    *at_uint16(group->set, i) = i;
    if(i == 0) {
      *at_uint16(group->invs, i) = 0;
    } else {
      *at_uint16(group->invs, i) = n - i;
    }
  }
  for(i = 0; i < n; i++) {
    for(j = 0; j < n; j++) {
      *at_uint16(group->mtab, i * n + j) = (i + j) % n;
    }
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
  for(i = 0; i < n; i++) {
    *at_uint16(group->invs, i) = compInv(group, i);
  }
  return group;
}

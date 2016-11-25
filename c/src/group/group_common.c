#include "group_common.h"

void createCn(Group *group) {
  uint16_t n = order(group);
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
}

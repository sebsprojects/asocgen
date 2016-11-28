#include "group_gen.h"

void generateFrom_noalloc(Group *group, Array_uint16 *set, Array_uint16 *res)
{
  uint16_t n = order(group);
#ifdef BOUNDS_CHECK
  if(res->size != n) {
    printError("error: generFrom_noalloc needs array of size order(group)!");
  }
#endif
  uint32_t i, j;
  uint16_t ele;

  // zero out result array
  for(i = 0; i < n; i++) {
    *at_uint16(res, i) = 0xffff;
  }
  // fill in initial elements from set with res[i] = i;
  for(i = 0; i < set->size; i++) {
    ele = *at_uint16(set, i);
    *at_uint16(res, ele) = ele;
  }

  uint16_t a, b;
  uint32_t prevFilled = 0;
  uint32_t filled = set->size;

  if(!isCommutative(group)) { // Not commutative, do both ab, ba
    while(filled != prevFilled) {
      prevFilled = filled;
      for(i = 0; i < n; i++) {
        for(j = 0; j <= i; j++) {
          a = *at_uint16(res, i);
          if(a == 0xffff) continue;
          b = *at_uint16(res, j);
          if(b == 0xffff) continue;

          ele = gop(group, a, b);
          if(*at_uint16(res, ele) == 0xffff) filled++;
          *at_uint16(res, ele) = ele;

          ele = gop(group, b, a);
          if(*at_uint16(res, ele) == 0xffff) filled++;
          *at_uint16(res, ele) = ele;
        }
      }
    }
  } else { // Commutative, so only do ab (omit ba)
    while(filled != prevFilled) {
      prevFilled = filled;
      for(i = 0; i < n; i++) {
        for(j = 0; j <= i; j++) {
          a = *at_uint16(res, i);
          if(a == 0xffff) continue;
          b = *at_uint16(res, j);
          if(b == 0xffff) continue;

          ele = gop(group, a, b);
          if(*at_uint16(res, ele) == 0xffff) filled++;
          *at_uint16(res, ele) = ele;
        }
      }
    }
  }
}

Array_uint16 *generateFrom_toset(Group *group, Array_uint16 *set) {
  Array_uint16 *res = allocArray_uint16(order(group));
  generateFrom_noalloc(group, set, res);
  return res;
}

void truncGeneratedSet(Group *group, Array_uint16 *res) {
  uint32_t n = order(group);
  uint32_t i, j;
  uint16_t *a, *b;
  uint16_t last = 0;
  for(i = 0; i < n; i++) {
    a = at_uint16(res, i);
    if(*a != 0xffff) {
      last = i;
      continue;
    }
    for(j = i + 1; j < n; j++) {
      b = at_uint16(res, j);
      if(*b == 0xffff) {
        continue;
      } else {
        *a = *b;
        *b = 0xffff;
        last = i;
        break;
      }
    }
  }
  shrink_uint16(res, last + 1);
}

Group *generateFrom_togroup(Group *group, Array_uint16 *set) {
  Array_uint16 *res = generateFrom_toset(group, set);
  truncGeneratedSet(group, res);
  uint32_t m = res->size;    // Subgroup order
  Group *subgroup = allocGroup(m);
  subgroup->indexed = 0;
  uint32_t i, j;
  uint16_t a, b;
  for(i = 0; i < m; i++) {
    a = *at_uint16(res, i);
    *at_uint16(subgroup->set, i) = a;
    *mapTo_uint16(subgroup->imap, i) = a;
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

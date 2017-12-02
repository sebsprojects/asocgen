#include "group_common.h"

Group *createFromGen(uint32_t n, Array_uint8 *gtab) {
  Group *group = group_alloc(n, 1);
  uint32_t i;
  for(i = 0; i < n; i++) {
    *at_uint16(group->set, i) = i;
  }
  for(i = 0; i < n * n; i++) {
    *at_uint16(group->gtab, i) = *at_uint8(gtab, i);
  }
  group_setInvs(group);
  return group;
}

Group *createCn(uint32_t n) {
  Group *group = group_alloc(n, 1);
  uint32_t i, j;
  for(i = 0; i < n; i++) {
    *at_uint16(group->set, i) = i;
    for(j = 0; j <= i; j++) {
      *at_uint16(group->gtab, get2DIndex(n, i, j)) = (i + j) % n;
      *at_uint16(group->gtab, get2DIndex(n, j, i)) = (j + i) % n;
    }
    *at_uint16(group->invs, i) = (n - i) % n;
  }
  return group;
}

Group *createSn(uint32_t n) {
#ifdef BOUNDS_CHECK
  if(n > 8) {
    printError("error: can only create Sn up to n <= 8");
    exit(1);
  }
#endif
  uint32_t nfac =  factorial(n);
  Array_uint16 *perm = allocArray_uint16(n);
  Array_uint16 *domain = allocArray_uint16(n);
  initPerm(perm);
  initPerm(domain); // not really perm but should be 0 1 2 ... n anyway
  uint32_t i;
  Group *group = group_alloc(nfac, 1);
  for(i = 0; i < nfac; i++) {
    *at_uint16(group->set, i) = i;
  }
  // Alloc
  Map_uint16 **mapArray = malloc(sizeof(Array_uint16*) * nfac);
  Array_uint16 *codomain = 0;
  for(i = 0; i < nfac; i++) {
    codomain = copyArray_uint16(perm);
    mapArray[i] =  allocMap_ref_uint16(n, 1, domain, codomain);
    shiftPerm(perm);
  }
  // Gtab
  Map_uint16 *prod = allocMap_uint16(n, 1);
  uint32_t j, k;
  for(i = 0; i < nfac; i++) {
    for(j = 0; j < nfac; j++) {
      if(i == 0) {
        *at_uint16(group->gtab, get2DIndex(nfac, i, j)) = j;
      } else if(j == 0) {
        *at_uint16(group->gtab, get2DIndex(nfac, i, j)) = i;
      } else {
        compMaps_noalloc(mapArray[i], mapArray[j], prod, 1);
        for(k = 0; k < nfac; k++) {
          if(areEqualArrays_uint16(mapArray[k]->codomain, prod->codomain)) {
            *at_uint16(group->gtab, get2DIndex(nfac, i, j)) = k;
            break;
          }
        }
      }
    }
  }
  group_setInvs(group);

  // Free
  freeMap_uint16(prod);
  for(i = 0; i < nfac; i++) {
    freeArray_uint16(mapArray[i]->codomain);
    freeMap_ref_uint16(mapArray[i]);
  }
  free(mapArray);
  freeArray_uint16(domain);
  freeArray_uint16(perm);
  return group;
}

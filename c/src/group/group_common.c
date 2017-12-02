#include "group_common.h"

Group *createFromGen(uint32_t n, Array_uint8 *gtab) {
  Group *group = group_alloc(n, 1);
  uint32_t i;
  for(i = 0; i < n; i++) {
    *aui16_at(group->set, i) = i;
  }
  for(i = 0; i < n * n; i++) {
    *aui16_at(group->gtab, i) = *aui8_at(gtab, i);
  }
  group_setInvs(group);
  return group;
}

Group *createCn(uint32_t n) {
  Group *group = group_alloc(n, 1);
  uint32_t i, j;
  for(i = 0; i < n; i++) {
    *aui16_at(group->set, i) = i;
    for(j = 0; j <= i; j++) {
      *aui16_at(group->gtab, get2DIndex(n, i, j)) = (i + j) % n;
      *aui16_at(group->gtab, get2DIndex(n, j, i)) = (j + i) % n;
    }
    *aui16_at(group->invs, i) = (n - i) % n;
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
  Array_uint16 *perm = aui16_alloc(n);
  Array_uint16 *domain = aui16_alloc(n);
  initPerm(perm);
  initPerm(domain); // not really perm but should be 0 1 2 ... n anyway
  uint32_t i;
  Group *group = group_alloc(nfac, 1);
  for(i = 0; i < nfac; i++) {
    *aui16_at(group->set, i) = i;
  }
  // Alloc
  Map_uint16 **mapArray = malloc(sizeof(Array_uint16*) * nfac);
  Array_uint16 *codomain = 0;
  for(i = 0; i < nfac; i++) {
    codomain = aui16_copy(perm);
    mapArray[i] = mapui16_alloc_ref(n, 1, domain, codomain);
    shiftPerm(perm);
  }
  // Gtab
  Map_uint16 *prod = mapui16_alloc(n, 1);
  uint32_t j, k;
  for(i = 0; i < nfac; i++) {
    for(j = 0; j < nfac; j++) {
      if(i == 0) {
        *aui16_at(group->gtab, get2DIndex(nfac, i, j)) = j;
      } else if(j == 0) {
        *aui16_at(group->gtab, get2DIndex(nfac, i, j)) = i;
      } else {
        mapui16_comp_noalloc(mapArray[i], mapArray[j], prod, 1);
        for(k = 0; k < nfac; k++) {
          if(aui16_areEqualArrays(mapArray[k]->codomain, prod->codomain)) {
            *aui16_at(group->gtab, get2DIndex(nfac, i, j)) = k;
            break;
          }
        }
      }
    }
  }
  group_setInvs(group);

  // Free
  mapui16_free(prod);
  for(i = 0; i < nfac; i++) {
    aui16_free(mapArray[i]->codomain);
    mapui16_free_ref(mapArray[i]);
  }
  free(mapArray);
  aui16_free(domain);
  aui16_free(perm);
  return group;
}

#include "group_common.h"

#include <math.h>

Group *createFromGen(uint32_t n, Array_uint8 *gtab) {
  Group *group = group_alloc(n, 1);
  uint32_t i;
  for(i = 0; i < n; i++) {
    *aui16_at(group->set, i) = i;
  }
  for(i = 0; i < n * n; i++) {
    *aui16_at(group->gtab, i) = *aui8_at(gtab, i);
  }
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
  perm_init(perm);
  aui16_setToRange(domain, 0, domain->size, 0);
  uint32_t i;
  Group *group = group_alloc(nfac, 1);
  for(i = 0; i < nfac; i++) {
    *aui16_at(group->set, i) = i;
  }
  // Alloc
  Map_uint16 **mapArray = malloc(sizeof(Map_uint16*) * nfac);
  Array_uint16 *codomain = 0;
  for(i = 0; i < nfac; i++) {
    codomain = aui16_copy(perm);
    mapArray[i] = mapui16_alloc_ref(n, 1, domain, codomain);
    perm_shiftDefault(perm);
  }
  // sort
  int32_t permutationCompare(const void *a, const void *b);
  qsort(mapArray, nfac, sizeof(Map_uint16*), permutationCompare);
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

// --------------------------------------------------------------------------

// only works for map->codomain in { 0..8 }, i.e. S9 (might chg to uint64_t)
uint32_t notfixedImagesToInt(Map_uint16 *map) {
  uint32_t res = 0;
  uint32_t expon = 0;
  int32_t i;
  uint16_t ele;
  for(i = map->domain->size - 1; i >= 0 ; i--) { // to have non-reversed int
    ele = mapui16_mapInd(map, i);
    if(ele != *aui16_at(map->domain, i)) {
      res += ele * pow(10, expon);
      expon++;
    }
  }
  return res;
}

/*
  Orders Permutation as follows
    1. If perm1 in S_n and perm2 in S_m then n < m translates (ignore fixed
       points)
    2. If equal, notFixedPointsToInt(perm1) < notFixedPointsToInt(perm2)
 */
int32_t permutationCompare(const void *a, const void *b) {
  Map_uint16 **perm1 = (Map_uint16**) a;
  Map_uint16 **perm2 = (Map_uint16**) b;
  // check if id
  if(!mapui16_hasNotfixedPoints(*perm1)) {
    return -1;
  }
  if(!mapui16_hasNotfixedPoints(*perm2)) {
    return 1;
  }
  uint16_t max1 = mapui16_getMaximalNotfixedImage(*perm1);
  uint16_t max2 = mapui16_getMaximalNotfixedImage(*perm2);
  if(max1 != max2) {
    return max1 - max2;
  } else {
    return notfixedImagesToInt(*perm1) - notfixedImagesToInt(*perm2);
  }
}

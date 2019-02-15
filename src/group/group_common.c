#include "group_common.h"

#include <elfc_math.h>
#include <elfc_vecptr.h>
#include <elfc_perm.h>

#include <math.h>
#include <stdlib.h>


Group *group_createCn_alloc(u32 n)
{
  Group *group = group_alloc(n, 1);
  for(i32 i = 0; i < n; i++) {
    *vecu16_at(group->set, i) = i;
    for(i32 j = 0; j <= i; j++) {
      *vecu16_at(group->gtab, get2DIndex(n, i, j)) = (i + j) % n;
      *vecu16_at(group->gtab, get2DIndex(n, j, i)) = (j + i) % n;
    }
  }
  return group;
}

Group *group_createDirectProduct_alloc(Group *a, Group *b)
{
  u32 n = group_order(a);
  u32 m = group_order(b);
  Group *prod = group_alloc(n * m, 1);
  vecu16_setToRange(prod->set, 0, n * m, 0);
  for(i32 i = 0; i < n; i++) {
    for(i32 j = 0; j < m; j++) {
      u16 ij = i * m + j;
      for(i32 k = 0; k < n; k++) {
        for(i32 l = 0; l < m; l++) {
          u16 kl = k * m + l;
          // computing the product of ij * kl
          u16 prodA = group_opi(a, i, k);
          u16 prodB = group_opi(b, j, l);
          *vecu16_at(prod->gtab, ij * m * n + kl) = prodA * m + prodB;
        }
      }
    }
  }
  return prod;
}


// ---------------------------------------------------------------------------
// Sn
// ---------------------------------------------------------------------------

Group *group_createSn_alloc(u32 n) {
//#ifdef BOUNDS_CHECK
//  if(n > 8) {
//    printError("error: can only create Sn up to n <= 8");
//    exit(1);
//  }
//#endif
  u32 nfac =  math_factorial(n);
  Vecu16 *perm = vecu16_alloc(n);   // permutation to iterate the elements
  Vecu16 *domain = vecu16_alloc(n); // same domain for all elements
  perm_init(perm);
  vecu16_setToRange(domain, 0, domain->size, 0);
  Group *group = group_alloc(nfac, 1);
  for(u16 i = 0; i < nfac; i++) {
    *vecu16_at(group->set, i) = i;
  }
  // alloc
  Vecptr *mapVec = vecptr_alloc(nfac);
  Vecu16 *codomain = 0;
  for(i32 i = 0; i < nfac; i++) {
    codomain = vecu16_copy(perm);
    *vecptr_at(mapVec, i) = mapu16_alloc_ref(domain, codomain, 1);
    perm_shiftDefault(perm);
  }
  // sort
  i32 permutationCompare(const void *a, const void *b);
  qsort(mapVec->data, nfac, sizeof(Mapu16*), permutationCompare);
  // gtab
  Mapu16 *prod = mapu16_alloc(n, 1);
  Mapu16 *kmap = 0;
  for(u16 i = 0; i < nfac; i++) {
    for(u16 j = 0; j < nfac; j++) {
      if(i == 0) { // prod with id
        *vecu16_at(group->gtab, get2DIndex(nfac, i, j)) = j;
      } else if(j == 0) { // prod with id
        *vecu16_at(group->gtab, get2DIndex(nfac, i, j)) = i;
      } else { // compute product of indices i * j
        mapu16_comp_noalloc(*vecptr_at(mapVec, i), *vecptr_at(mapVec, j),
                            prod, 1);
        // get index of prod in mapVec
        for(u16 k = 0; k < nfac; k++) {
          kmap = *vecptr_at(mapVec, k);
          if(vecu16_areEqualVectors(kmap->codomain, prod->codomain)) {
            *vecu16_at(group->gtab, get2DIndex(nfac, i, j)) = k;
            break;
          }
        }
      }
    }
  }
  // Free
  mapu16_free(prod);
  for(i32 i = 0; i < nfac; i++) {
    Mapu16 *map = *vecptr_at(mapVec, i);
    vecu16_free(map->codomain);
    mapu16_free_ref(map);
  }
  vecptr_free(mapVec);
  vecu16_free(domain);
  vecu16_free(perm);
  return group;
}

/*
 * Encodes the permutation into and uint. The highest fixed images are ignored
 * 0 1 2 3 4 5 6
 * 2 1 3 4 0 5 6
 * will be encoded as 21340
 */
i32 notfixedImagesToInt(Mapu16 *map)
{
  i32 res = 0;
  u32 expon = 0;
  u16 ele;
  bool afterFixed = 0;
  for(i32 i = map->domain->size - 1; i >= 0 ; i--) {
    ele = mapu16_mapInd(map, i);
    afterFixed = ele != *vecu16_at(map->domain, i);
    if(afterFixed) {
      res += ele * pow(10, expon);
      expon++;
    }
  }
  return res;
}

/*
 * Orders Permutation as follows
 *   1. If perm1 in S_n and perm2 in S_m then n < m translates (ignore fixed
 *      points) via fixed points in k > n
 *   2. If equal, notFixedPointsToInt(perm1) < notFixedPointsToInt(perm2)
 */
i32 permutationCompare(const void *a, const void *b)
{
  Mapu16 **perm1 = (Mapu16**) a;
  Mapu16 **perm2 = (Mapu16**) b;
  // check if id
  if(!mapu16_hasNotfixedPoints(*perm1)) {
    return -1;
  }
  if(!mapu16_hasNotfixedPoints(*perm2)) {
    return 1;
  }
  u16 max1 = mapu16_getMaximalNotfixedImage(*perm1);
  u16 max2 = mapu16_getMaximalNotfixedImage(*perm2);
  if(max1 != max2) {
    return max1 - max2;
  } else {
    return notfixedImagesToInt(*perm1) - notfixedImagesToInt(*perm2);
  }
}

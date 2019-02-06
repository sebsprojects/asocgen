#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <elfc_perm.h>
#include <elfc_veci32.h>
#include <elfc_mapu16.h>

#include "group/group.h"
#include "group/group_common.h"
#include "group/group_gen.h"
#include "group/group_hom.h"


i32 main() {
  u32 n = 6;
  Group *cn = createCn_alloc(n);
  Group *sn = createSn_alloc(n);
  u32 m = group_order(sn);

  Vecu16 *res = vecu16_alloc(m);
  Vecu16 *util1 = vecu16_alloc(m);
  Vecu16 *util2 = vecu16_alloc(m);
  Vecu16 *binom = vecu16_alloc(m);
  Vecu16 *orderConstr = vecu16_allocN(2, 2, 5);
  Vecu16 *groupOrders = vecu16_alloc(m);
  for(i32 i = 0; i < m; i++) {
    *vecu16_at(groupOrders, i) = group_elementOrderi(sn, i);
  }
  Vecu16 *genUtil = vecu16_alloc(2);
  GenOrderConstr constr;
  constr.orderConstr = orderConstr;
  constr.groupEleOrders = groupOrders;
  constr.util = genUtil;

  binom_init(binom, 0, 2, 0);
  bool ok = 1;
  u32 count = 0;
  Veci32 *ordCounter = veci32_alloc(100);
  veci32_fill(ordCounter, 0);
  u16 a = 0;
  u16 b = 0;
  u16 ordA = 0;
  u16 ordB = 0;
  u16 c = 0;
  //TODO: Somehow the orders do not match in group_gen function check
  //and orders checked here
  while(ok) {
    ok = group_minGeneratingSetConstr_noalloc(sn, res, binom,
                                              util1, util2, &constr);
    //ok = group_minGeneratingSet_noalloc(sn, res, binom, util1, util2);
    if(*vecu16_at(res, 0) == 0xffff || *vecu16_at(binom, 2) != 0xffff) {
      break;
    }
    count++;
    a = *vecu16_at(res, 0);
    b = *vecu16_at(res, 1);
    ordA = group_elementOrder(sn, a);
    ordB = group_elementOrder(sn, b);
    if(ordB < ordA) {
      c = ordA;
      ordA = ordB;
      ordB = c;
    }
    (*veci32_at(ordCounter, 10 * ordA + ordB))++;
    //printf("%i :: found genSet [ %i %i ] with order [ %i %i ]\n", count,
    //       a, b, ordA, ordB);
  }
  i32 r = 0;
  for(i32 i = 0; i < ordCounter->size; i++) {
    r = *veci32_at(ordCounter, i);
    if(r != 0) {
      printf("GenSet with ord [ %i %i ] :: %i\n",
             i / 10, i % 10, r);
    }
  }

  veci32_free(ordCounter);
  vecu16_free(orderConstr);
  vecu16_free(groupOrders);
  vecu16_free(genUtil);
  vecu16_free(binom);
  vecu16_free(util2);
  vecu16_free(util1);
  vecu16_free(res);

  group_free(sn);
  group_free(cn);
  return 0;
}

void misc2()
{
/*
  Vecu16 *res = vecu16_alloc(m);
  Vecu16 *util1 = vecu16_alloc(m);
  Vecu16 *util2 = vecu16_alloc(m);
  Vecu16 *binom = vecu16_alloc(m);

  binom_init(binom, 0, 2, 0);
  bool ok = 1;
  u32 ind = -1;
  u32 count = 0;
  Veci32 *ordCounter = veci32_alloc(100);
  veci32_fill(ordCounter, 0);
  u16 a = 0;
  u16 b = 0;
  u16 ordA = 0;
  u16 ordB = 0;
  u16 c = 0;
  while(ok) {
    group_minGeneratingSet_noalloc(sn, res, binom, util1, util2);
    count++;
    a = *vecu16_at(res, 0);
    b = *vecu16_at(res, 1);
    ordA = group_elementOrder(sn, a);
    ordB = group_elementOrder(sn, b);
    if(ordB < ordA) {
      c = ordA;
      ordA = ordB;
      ordB = c;
    }
    (*veci32_at(ordCounter, 10 * ordA + ordB))++;
    printf("%i :: found genSet [ %i %i ] with order [ %i %i ]\n", count,
           a, b, ordA, ordB);
    vecu16_indexOf(binom, 0xffff, &ind, 0);
    ok = ind < 3;
  }
  i32 r = 0;
  for(i32 i = 0; i < ordCounter->size; i++) {
    r = *veci32_at(ordCounter, i);
    if(r != 0) {
      printf("GenSet with ord [ %i %i ] :: %i\n",
             i / 10, i % 10, r);
    }
  }

  veci32_free(ordCounter);
  vecu16_free(binom);
  vecu16_free(util2);
  vecu16_free(util1);
  vecu16_free(res);
*/
}

void misc1()
{
  /*
  Vecptr *decompVec = vecptr_alloc(m);
  for(i32 i = 0; i < m; i++) {
    *vecptr_at(decompVec, i) = vecu16_alloc(m * 2); // TODO check in hom
  }
  group_genDecomposition(sncopy, genSet, decompVec);
  u32 ind;
  u32 longestDecomp = 0;
  u32 longestDecompInd = 0;
  Vecu16 *decomp;
  for(i32 i = 0; i < m; i++) {
    //vecu16_print(*vecptr_at(decompVec, i));
    printf("At index i=%i where g=%i\n", i, *vecu16_at(sncopy->set, i));
    decomp = *vecptr_at(decompVec, i);
    vecu16_indexOf(decomp, 0xffff, &ind, 0);
    printf("  Length of decomp: %i\n", ind);
    if(longestDecomp < ind) {
      longestDecomp = ind;
      longestDecompInd = i;
    }
    u16 prod = *vecu16_at(decomp, 0);
    for(i32 j = 1; j < ind; j++) {
      prod = group_op(sncopy, prod, *vecu16_at(decomp, j));
    }
    printf("  Product: %i\n\n", prod);
  }
  printf("Longest Decomp: %i\n", longestDecomp);
  vecu16_print(*vecptr_at(decompVec, longestDecompInd));
  for(i32 i = 0; i < m; i++) {
    vecu16_free(*vecptr_at(decompVec, i));
  }
  vecptr_free(decompVec);
  */
}

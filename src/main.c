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

i32 main_2();
i32 main_1();

i32 main() {
  return main_2();
}

i32 main_2() {
  // Create Sn
  u32 n = 5;
  Group *sn = createSn_alloc(n);
  u32 m = group_order(sn);

  // Create Sn re-named
  Mapu16 *map = mapu16_alloc(m, sn->indexed);
  vecu16_copyInto(sn->set, map->domain, 0);
  *vecu16_at(map->codomain, 0) = *vecu16_at(map->domain, 0);
  for(i32 i = 1; i < m; i++) {
    *vecu16_at(map->codomain, i) = *vecu16_at(map->domain, m - i);
  }
  Group *snCopy = group_getRenamedCopy_alloc(sn, map);
  mapu16_free(map);

  Vecu16 *binom = vecu16_alloc(m);
  Vecu16 *orderConstr = vecu16_allocN(2, 2, 5);
  Vecu16 *genFrom = vecu16_allocN(m);

  // Find getFrom
  GenConstrUtils *constrUtils = group_allocSetupConstrUtils(sn, 2);
  binom_init(binom, 0, 2, 0);
  group_minGeneratingSetConstr(sn, genFrom, binom, orderConstr, constrUtils);
  group_freeConstrUtils(constrUtils);
  u32 gs = genFrom->size;
  vecu16_indexOf(genFrom, 0xffff, &gs, 0);
  vecu16_resize(genFrom, gs);
  vecu16_print(genFrom);

  // Init Hom and IsoUtils
  Mapu16 *homMap = mapu16_alloc(m, sn->indexed);
  vecu16_copyInto(sn->set, homMap->domain, 0);
  GroupHom *hom = group_allocHom_ref(sn, snCopy, homMap);
  HomIsoUtils *isoUtils = group_allocSetupIsoUtils(hom, genFrom, orderConstr);
  binom_init(binom, 0, 2, 0);
  bool ok = 1;
  while(ok) {
    ok = group_checkForIsomorphismFromGen(hom, genFrom, binom, isoUtils);
  }

  group_freeHom_ref(hom);
  mapu16_free(homMap);
  group_free(snCopy);

  vecu16_free(genFrom);
  group_freeIsoUtils(isoUtils);
  vecu16_free(orderConstr);
  vecu16_free(binom);
  group_free(sn);
  return 0;
}

i32 main_1() {
  u32 n = 5;
  Group *sn = createSn_alloc(n);
  u32 m = group_order(sn);

  Vecu16 *res = vecu16_alloc(m);
  Vecu16 *binom = vecu16_alloc(m);
  Vecu16 *orderConstr = vecu16_allocN(2, 2, 5);
  GenConstrUtils *constr = group_allocSetupConstrUtils(sn, 2);

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
    //ok = group_minGeneratingSetConstr(sn, res, binom, orderConstr, constr);
    ok = group_minGeneratingSet_noalloc(sn, res, binom,
                                        constr->genUtil1,
                                        constr->genUtil2);
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

  group_freeConstrUtils(constr);
  vecu16_free(orderConstr);

  vecu16_free(binom);
  vecu16_free(res);

  group_free(sn);
  return 0;
}

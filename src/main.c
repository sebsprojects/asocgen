#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <elfc_perm.h>
#include <elfc_veci32.h>
#include <elfc_mapu16.h>
#include <elfc_hash.h>

#include "group/group.h"
#include "group/group_common.h"
#include "group/group_gen.h"
#include "group/group_hom.h"
#include "group/group_io.h"

i32 main_5();
i32 main_4();
i32 main_3();
i32 main_2();
i32 main_1();


i32 main()
{
  return main_5();
}

i32 main_5()
{
  Group *c = group_createSn_alloc(5);

  u16 n = group_order(c);
  f64 baseLog16 = 1.0 / log(16.0);
  u16 maxEleLen = floor(log((f64) n) * baseLog16) + 1;
  char *buf = malloc(n * n * maxEleLen + 100);
  char *fnameBuf = malloc(100);
  char *headerBuf = malloc(1000);
  buf[0] = '\0';
  fnameBuf[0] = '\0';
  headerBuf[0] = '\0';
  group_sprintGTab(buf, c);

  u64 hash = hash_djb2(buf);

  group_sprintFileName(fnameBuf, c, hash);

  GroupMetaInfo cInfo;
  cInfo.name ="S5";
  cInfo.djb2Hash = hash;
  cInfo.order = group_order(c);
  cInfo.isCommutative = group_isCommutative(c);
  cInfo.minGenSet = 0;
  group_sprintHeader(headerBuf, cInfo);

  printf("%s\n", fnameBuf);
  printf("\n%s\n", headerBuf);

  free(headerBuf);
  free(fnameBuf);
  free(buf);

  group_free(c);
  return 0;
}

i32 main_4()
{
  Group *c6 = group_createSn_alloc(3);
  Group *c9 = group_createCn_alloc(9);
  Group *p = group_createDirectProduct_alloc(c6, c9);

  group_printMinGenSetOrderDist(p);

  group_free(p);
  group_free(c6);
  group_free(c9);

  return 0;
}

i32 main_3()
{
  Group *c3 = group_createCn_alloc(3);
  Group *c18 = group_createCn_alloc(18);
  //Group *c6 = group_createCn_alloc(6);
  Group *c6 = group_createSn_alloc(3);
  Group *c9 = group_createCn_alloc(9);
  Group *p1 = group_createDirectProduct_alloc(c3, c18);
  Group *p2 = group_createDirectProduct_alloc(c6, c9);

  u32 n = group_order(p1);

  Vecu16 *genFrom = group_minGeneratingSet_alloc(p1);
  u32 genFromS = genFrom->size;
  vecu16_indexOf(genFrom, 0xffff, &genFromS, 0);
  vecu16_resize(genFrom, genFromS);
  Vecu16 *binom = vecu16_alloc(n);
  Mapu16 *map = mapu16_alloc(group_order(p1), p1->indexed);
  vecu16_copyInto(p1->set, map->domain, 0);
  GroupHom *hom = group_allocHom_ref(p1, p2, map);
  HomIsoUtils *isoUtils = group_allocSetupIsoUtils(hom, genFrom);
  binom_init(binom, 0, genFrom->size, 0);

  vecu16_print(genFrom);
  vecu16_print(isoUtils->genFromOrders);

  bool ok = 1;
  while(ok) {
    ok = group_checkForIsomorphismFromGen(hom, genFrom, binom, isoUtils);
  }

  group_freeIsoUtils(isoUtils);
  group_freeHom_ref(hom);
  mapu16_free(map);
  vecu16_free(binom);
  vecu16_free(genFrom);

  group_free(p2);
  group_free(p1);
  group_free(c9);
  group_free(c6);
  group_free(c18);
  group_free(c3);

  return 0;
}

i32 main_2()
{
  // Create Sn
  u32 n = 5;
  Group *sn = group_createSn_alloc(n);
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
  Vecu16 *orderConstr = vecu16_allocN(2, 4, 4);
  Vecu16 *genFrom = vecu16_alloc(m);

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
  HomIsoUtils *isoUtils = group_allocSetupIsoUtils(hom, genFrom);
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
  Group *sn = group_createSn_alloc(n);
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

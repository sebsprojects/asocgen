#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <elfc_perm.h>
#include <elfc_veci32.h>
#include <elfc_mapu16.h>
#include <elfc_hash.h>
#include <elfc_random.h>

#include "group/group.h"
#include "group/group_common.h"
#include "group/group_gen.h"
#include "group/group_hom.h"
#include "group/group_io.h"

#include "application/group_library.h"

i32 main_11();
i32 main_10();
i32 main_9();
i32 main_8();
i32 main_7();
i32 main_6();
i32 main_5();
i32 main_4();
i32 main_3();
i32 main_2();
i32 main_1();

i32 main()
{
  return main_11();
}

i32 main_11()
{
  //Group *g1 = group_readGroupFromFile_alloc("./grplib/00016n_46544f7fed32b725.txt");
  //Group *g2 = group_readGroupFromFile_alloc("./grplib/00016n_ad667b22219eb725.txt");
  //Group *g1 = group_readGroupFromFile_alloc("./grplib/00048n_37e12bcbfe003125.txt");
  //Group *g2 = group_readGroupFromFile_alloc("./grplib/00048n_7b17045886823125.txt");
  Group *g1 = group_readGroupFromFile_alloc("./grplib/00120n_21e80f54a2599d75.txt");
  Group *g2 = group_readGroupFromFile_alloc("./grplib/00120n_b916c3ae173f1d75.txt");
  if(g1 == 0 || g2 == 0) {
    return 1;
  }
  u16 n = group_order(g1);
  Mapu16 *om1 = group_orderDist_alloc(g1);
  Mapu16 *om2 = group_orderDist_alloc(g2);
  vecu16_print(om1->domain);
  vecu16_print(om1->codomain);
  vecu16_print(om2->domain);
  vecu16_print(om2->codomain);
  mapu16_free(om1);
  mapu16_free(om2);
  Vecu16 *genFrom = group_minGeneratingSet_alloc(g1);
  group_truncGeneratedSet(genFrom, 1);
  Mapu16 *h = mapu16_alloc(n, 1);
  vecu16_copyInto(g1->set, h->domain, 0);
  GroupHom *hom = group_allocHom_ref(g1, g2, h);
  Vecu16 *binom = vecu16_alloc(n);
  binom_init(binom, 0, genFrom->size, 0);
  HomIsoUtils *isoUtils = group_allocSetupIsoUtils(hom, genFrom);
  vecu16_print(genFrom);
  vecu16_print(isoUtils->genFromOrders);
  bool ok = 1;
  while(ok) {
    ok = group_checkForIsomorphismFromGen(hom, genFrom, binom, isoUtils);
  }

  group_freeIsoUtils(isoUtils);
  group_freeHom_ref(hom);
  mapu16_free(h);
  vecu16_free(binom);
  vecu16_free(genFrom);

  group_free(g2);
  group_free(g1);
  return 0;
}

i32 main_10()
{
  Group *group =
    group_readGroupFromFile_alloc("./grplib/05040n_ec6eb6f39e278625.txt");
  if(group == 0) {
    return 0;
  }
  rand_setSeed(444);
  Group *g = app_searchForGroup_alloc(group, 120, 2, 3, 90000);
  printf("Found valid subgroup: %u\n", group_isValid(g));
  printf("Writing subgroup to file: %u\n", app_writeGroup("./grplib", g, 0));
  group_free(g);
  group_free(group);
  return 0;
}

i32 main_9()
{
  Vecptr *fns = app_listGroupFiles_alloc("./grplib", 24, 0);
  for(i32 i = 0; i < fns->size; i++) {
    printf("File name: %s\n", (char *) *vecptr_at(fns, i));
    free(*vecptr_at(fns, i));
  }
  vecptr_free(fns);
  return 0;
}

i32 main_8()
{
  GroupMetaInfo m = group_readMetaFromFile("./grplib/00024n_078a417cf4412f95.txt");
  printf("meta.name: :%s:\n", m.name);
  printf("meta.order: :%u:\n", m.order);
  printf("meta.isCommutative: :%u:\n", m.isCommutative);
  printf("meta.minGenSet: ");
  for(i32 i = 0; i < m.minGenSetSize; i++) {
    printf("%u ", m.minGenSet[i]);
  }
  printf("\n");
  return 0;
}

i32 main_7()
{
  Group *g = group_readGroupFromFile_alloc("./groups/00024n_078a417cf4412f95.txt");
  if(g == 0) {
    printf("Error while reading group file\n");
  }
  group_printSummary(g);
  group_free(g);
  return 0;
}

i32 main_6()
{
  Group *g = group_createSn_alloc(4);

  Vecu16 *mgs = group_minGeneratingSet_alloc(g);
  group_truncGeneratedSet(mgs, 1);
  GroupMetaInfo meta;
  memcpy(meta.name, "S5", 3);
  meta.order = group_order(g);
  meta.isCommutative = group_isCommutative(g);
  meta.minGenSetSize = mgs->size;
  vecu16_copyIntoArray(meta.minGenSet, mgs, mgs->size);

  group_writeIndexedToFile(g, meta, ".");

  vecu16_free(mgs);
  group_free(g);
  return 0;
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
  cInfo.order = group_order(c);
  cInfo.isCommutative = group_isCommutative(c);
  group_sprintHeader(headerBuf, cInfo);

  printf("%s\n", fnameBuf);
  printf("\n%s\n", headerBuf);
  printf("%s\n", buf);

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

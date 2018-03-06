#include "test.h"

#include "../src/group/group_common.h"


void test_notfixedpoints() {
  printTestHead("map", "notfixedpoints");
  bool ok = 1;
  Map_uint16 *map = mapui16_alloc(6, 1);
  mapui16_toId(map);
  *aui16_at(map->codomain, 2) = 9;
  *aui16_at(map->codomain, 3) = 1;
  *aui16_at(map->codomain, 5) = 2;
  mapui16_printDefault(map);
  ok &= mapui16_hasNotfixedPoints(map);
  ok &= mapui16_getMaximalNotfixedImage(map) == 9;
  //ok &= notfixedImagesToInt(map) == 912;
  //printf("int from notfixedpoints: %u\n", notfixedImagesToInt(map));
  mapui16_free(map);
  printTestFoot(ok);
}

void test_print() {
  printTestHead("map", "print");
  bool ok = 1;
  Map_uint16 *map = mapui16_alloc(20, 1);
  mapui16_toId(map);
  *aui16_at(map->codomain, 4) = 0xffff;
  mapui16_printToWidth(map, 50, 10);
  mapui16_free(map);
  printTestFoot(ok);
}

void test_permComp() {
  printTestHead("map", "perm comp");
  bool ok = 1;
  uint32_t n = 3;
  Map_uint16 *f = mapui16_alloc(n, 1);
  Map_uint16 *g = mapui16_alloc(n, 1);
  mapui16_toId(f);
  mapui16_toId(g);
  *aui16_at(f->codomain, 0) = 2; // 120
  *aui16_at(f->codomain, 1) = 1;
  *aui16_at(f->codomain, 2) = 0;
  *aui16_at(g->codomain, 0) = 2; // 201
  *aui16_at(g->codomain, 1) = 0;
  *aui16_at(g->codomain, 2) = 1;
  mapui16_printDefault(f);
  mapui16_printDefault(g);
  printf("  Compare: %i\n", permutationCompare(&f, &g));
  mapui16_free(f); mapui16_free(g);
  printTestFoot(ok);
}

void test_permSort() {
  printTestHead("map", "perm sort");
  bool ok = 1;
  uint32_t n = 3;
  uint32_t nfac =  factorial(n);
  Array_uint16 *perm = aui16_alloc(n);
  Array_uint16 *domain = aui16_alloc(n);
  aui16_setToRange(perm, 0, perm->size, 0);
  aui16_setToRange(domain, 0, domain->size, 0);
  // Alloc
  Map_uint16 **mapArray = malloc(sizeof(Map_uint16*) * nfac);
  Array_uint16 *codomain = 0;
  uint32_t i;
  for(i = 0; i < nfac; i++) {
    codomain = aui16_copy(perm);
    mapArray[i] = mapui16_alloc_ref(n, 1, domain, codomain);
    perm_shiftDefault(perm);
  }
  // sort
  int32_t permutationCompare(const void *a, const void *b);
  qsort(mapArray, nfac, sizeof(Map_uint16*), permutationCompare);

  for(i = 0; i < nfac; i++) {
    mapui16_printDefault(mapArray[i]);
    printf("\n");
  }

  // Free
  for(i = 0; i < nfac; i++) {
    aui16_free(mapArray[i]->codomain);
    mapui16_free_ref(mapArray[i]);
  }
  free(mapArray);
  aui16_free(domain);
  aui16_free(perm);
  printTestFoot(ok);
}

void test_mapArray() {
  printTestHead("map", "array");
  bool ok = 1;
  Array_uint16 *array = aui16_alloc5(0, 1, 2, 3, 0);
  Map_uint16 *map = mapui16_alloc(4, 1);
  mapui16_toId(map);
  *aui16_at(map->codomain, 0) = 10;
  *aui16_at(map->codomain, 1) = 20;
  *aui16_at(map->codomain, 2) = 30;
  *aui16_at(map->codomain, 3) = 40;
  mapui16_printDefault(map);
  aui16_printDefault(array);
  mapui16_mapArray(map, array, array);
  aui16_printDefault(array);
  mapui16_free(map);
  aui16_free(array);
  printTestFoot(ok);
}

void test_suite_map() {
  //test_notfixedpoints();
  //test_print();
  //test_permComp();
  //test_permSort();
  test_mapArray();
}

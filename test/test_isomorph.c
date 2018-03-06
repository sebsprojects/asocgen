#include "test.h"

#include "../src/ring/ring_includes.h"

void test_rename() {
  printTestHead("isomorph", "rename");
  bool ok = 1;
  Group *g = createSn(3);
  Map_uint16 *map = mapui16_alloc(6, 0);
  mapui16_toId(map);
  uint32_t i;
  for(i = 0; i < 6; i++) {
    *aui16_at(map->codomain, i) = 5 - i;
  }
  group_print(g);
  mapui16_printDefault(map);
  //
  Group *h = group_getRenamedCopy_alloc(g, map);
  group_print(h);
  GroupHom *hom = group_allocHom_ref(g, h, map);
  printf("Is valid hom %u\n", group_isValidHom(hom));
  printf("Is valid iso %u\n", group_isIsomorphism(hom));
  group_freeHom_ref(hom);
  Group *x = group_getIndexedCopy_alloc(h);
  group_print(x);
  group_free(x);
  group_free(h);
  //
  mapui16_free(map);
  group_free(g);
  printTestFoot(ok);
}

void test_add_to_mulCn() {
  printTestHead("isomorph", "add to mul");
  bool ok = 1;
  Group *add = createCn(13);
  Ring *z13z = createZnZ(13);
  Group *mul = ring_getUnitGroup_alloc(z13z);
  group_free(mul);
  ring_free(z13z);
  group_free(add);
  printTestFoot(ok);
}

void test_suite_isomorph() {
  test_rename();
  test_add_to_mulCn();
}

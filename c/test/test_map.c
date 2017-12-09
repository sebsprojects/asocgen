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

void test_suite_map() {
  test_notfixedpoints();
  test_print();
}

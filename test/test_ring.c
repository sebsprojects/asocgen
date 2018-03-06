#include "test.h"

#include "../src/ring/ring_includes.h"

void test_znz() {
  printTestHead("ring", "znz");
  bool ok = 1;
  uint32_t n = 14;
  Ring *ring = createZnZ(n);
  printf("  - Has one      : %u\n", ring_hasOne(ring));
  printf("  - Has add group: %u\n", ring_addIsCommutativeGroup(ring));
  printf("  - Has asoc mul : %u\n", ring_mulIsAssociative(ring));
  printf("  - Has distrib  : %u\n", ring_isDistributive(ring));
  printf("  ~ Is valid     : %u\n\n", ring_isValid(ring));
  printf("  - Is commutativ: %u\n", ring_isCommutative(ring));
  printf("  - Has zerodiv  : %u\n", ring_hasZeroDivisors(ring));
  printf("  - Is integral d: %u\n", ring_isIntegralDomain(ring));
  printf("  - Is field     : %u\n", ring_isField(ring));
  Group *ringX = ring_getUnitGroup_alloc(ring);
  group_print(ringX);
  group_free(ringX);
  ring_free(ring);
  printTestFoot(ok);
}

void test_suite_ring() {
  test_znz();
}

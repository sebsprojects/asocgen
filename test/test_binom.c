#include "test.h"

#include "../src/common/common_includes.h"


void test_binom_shift() {
  printTestHead("binom", "shift");
  bool ok = 1;
  Array_uint16 *binom = aui16_alloc5(0, 1, 2, 3, 4);
  bool go = 1;
  aui16_printDefault(binom);
  while(go) {
    go = binom_shift(binom, 0, 5, 0, 3);
    aui16_printDefault(binom);
  }
  go = 1;
  printf("-----\n");
  while(go) {
    go = binom_shift(binom, 3, 8, 3, 2);
    aui16_printDefault(binom);
  }
  aui16_free(binom);
  printTestFoot(ok);
}

void test_binom_trivial() {
  printTestHead("binom", "trivial");
  bool ok = 1;
  Array_uint16 *binom = aui16_alloc5(5, 6, 7, 8, 9);
  aui16_printDefault(binom);
  ok &= !binom_shiftDefault(binom, 9);
  ok &= !binom_shift(binom, 6, 8, 1, 3);
  aui16_free(binom);
  printTestFoot(ok);
}

void test_suite_binom() {
  test_binom_shift();
  test_binom_trivial();
}

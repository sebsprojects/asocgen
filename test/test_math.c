#include "test.h"

#include "../src/common/common.h"

void test_minmax() {
  printTestHead("math", "minmax");
  uint32_t a = 10; uint32_t b = 20; uint32_t c = 20;
  bool ok = 1;
  ok &= umin(a, b) == a;
  ok &= umax(a, b) == b;
  ok &= umin(b, c) == c;
  ok &= umin(c, b) == c;
  ok &= umax(b, c) == b;
  printTestFoot(ok);
}

void test_suite_math() {
  test_minmax();
}

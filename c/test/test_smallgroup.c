#include "test.h"

#include "../src/group/group_includes.h"

void test_s5() {
  printTestHead("group", "s5");
  Group *s5 = createSn(5);
  group_printSummary(s5);
  printTestFoot(group_isValid(s5));
  group_free(s5);
}

void test_c102() {
  printTestHead("group", "c102");
  Group *c102 = createCn(102);
  group_printSummary(c102);
  printTestFoot(group_isValid(c102));
  group_free(c102);
}

void test_mingen_s4() {
  printTestHead("group", "mingen_s4");
  Group *s4 = createSn(4);
  uint32_t n = group_order(s4);
  Array_uint16 *res = aui16_alloc(n);
  Array_uint16 *binom = aui16_alloc(n);
  Array_uint16 *util1 = aui16_alloc(n);
  Array_uint16 *util2 = aui16_alloc(n);
  initBinom(binom, 1);
  uint32_t count = 0;
  bool hasSucc = 1;
  uint32_t minSize = 0;
  while(hasSucc) {
    hasSucc = group_minGeneratingSet_noalloc(s4, res, binom, util1, util2);
    if(minSize > 0 && binom_getK(binom) > minSize) {
      break;
    }
    if(*aui16_at(res, 0) != 0xffff) {
      count++;
      printf("Generated s4 with");
      Group *gend = group_generateSubgroup_alloc(s4, res);
      bool isS4 = group_isValid(gend) && group_order(s4) == group_order(gend);
      group_free(gend);
      uint32_t j;
      uint16_t ele;
      for(j = 0; j < res->size; j++) {
        ele = *aui16_at(res, j);
        if(ele != 0xffff) {
          char pad[5];
          pad[0] = '\0';
          padStringForInt(pad, ele, 2);
          printf(" %s%u :: %u", pad, ele, group_elementOrder(s4, ele));
        }
      }
      printf(" -- isS4 %u\n", isS4);
      if(minSize == 0) {
        minSize = binom_getK(binom);
      }
    }
  }
  printf("Minimal size of generating set: %u\n", minSize);
  printf("Total number of minimal generating sets: %u\n", count);
  printTestFoot(count > 0);
  aui16_free(util2);
  aui16_free(util1);
  aui16_free(binom);
  aui16_free(res);
  group_free(s4);
}

void test_suite_smallgroup () {
  test_s5();
  test_c102();
  test_mingen_s4();
}

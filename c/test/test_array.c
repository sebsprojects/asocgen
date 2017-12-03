#include "test.h"

#include "../src/common/common_includes.h"


void test_basic_sort() {
  printTestHead("array16", "basic sort");
  Array_uint16 *array = aui16_alloc5(5, 4, 2, 0xffff, 2);
  aui16_sort(array, 0, array->size - 1);
  uint32_t i; uint16_t a, b; bool ok = 1;
  for(i = 1; i < array->size; i++) {
    a = *aui16_at(array, i - 1); b = *aui16_at(array, i);
    ok &= a <= b; // less or equal check
  }
  printTestFoot(ok);
  aui16_free(array);
}

void test_partial_sort() {
  printTestHead("array16", "partial sort");
  Array_uint16 *array = aui16_alloc5(5, 3, 2, 0xffff, 2);
  aui16_sort(array, 1, array->size - 2);
  uint32_t i; uint16_t a, b; bool ok = 1;
  for(i = 2; i < array->size - 1; i++) {
    a = *aui16_at(array, i - 1); b = *aui16_at(array, i);
    ok &= a <= b; // less or equal check
  }
  a = *aui16_at(array, 0); ok &= a == 5; // first element still same
  a = *aui16_at(array, 4); ok &= a == 2; // last element still same
  printTestFoot(ok);
  aui16_free(array);
}

void test_subset() {
  printTestHead("array16", "subset");
  Array_uint16 *base = aui16_alloc5(0, 1, 2, 2, 0xffff);
  Array_uint16 *ssub = aui16_alloc3(0, 1, 0xffff);
  Array_uint16 *dsub = aui16_alloc5(0, 1, 1, 1, 1);
  Array_uint16 *bsub = aui16_allocN(6, 0, 1, 2, 2, 2, 0xffff);
  Array_uint16 *nsub = aui16_alloc5(0, 1, 2, 2, 3);
  bool ok = 1;
  ok &= aui16_isSubset(base, base);  // subseq
  ok &= aui16_isSubset(ssub, base);  // standard subset
  ok &= aui16_isSubset(dsub, base);  // subset neglicting multiples
  ok &= aui16_isSubset(bsub, base);  // bigger array but subset
  ok &= !aui16_isSubset(nsub, base); // not subset
  ok &= !aui16_isSubset(ssub, dsub); // 0xffff is not treated specially
  printTestFoot(ok);
  aui16_free(nsub); aui16_free(bsub); aui16_free(dsub);
  aui16_free(ssub); aui16_free(base);
}

void test_equalsets() {
  printTestHead("array16", "setEqual");
  Array_uint16 *a1 = aui16_alloc5(0, 1, 2, 2, 0xffff);
  Array_uint16 *a2 = aui16_alloc4(0xffff, 2, 1, 0);
  Array_uint16 *a3 = aui16_alloc5(0, 1, 2, 3, 0xffff);
  Array_uint16 *a4 = aui16_alloc5(0, 1, 2, 2, 2);
  Array_uint16 *a5 = aui16_allocN(6, 0xffff, 0, 1, 2, 1, 0xffff);
  bool ok = 1;
  ok &= aui16_areEqualSets(a1, a1);  // simple
  ok &= aui16_areEqualSets(a1, a2);  // order, duplicates
  ok &= aui16_areEqualSets(a1, a5);  // duplicates
  ok &= !aui16_areEqualSets(a1, a3); // not equal
  ok &= !aui16_areEqualSets(a1, a4); // not equal
  printTestFoot(ok);
  aui16_free(a5); aui16_free(a4); aui16_free(a3);
  aui16_free(a2); aui16_free(a1);
}

void test_suite_array() {
  test_basic_sort();
  test_partial_sort();
  test_subset();
  test_equalsets();
}

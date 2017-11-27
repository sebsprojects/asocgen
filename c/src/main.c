#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gen/asocgen.h"
#include "common/common_includes.h"
#include "group/group_includes.h"

void example_1() {
  uint8_t n = 12;
  Zip *zip = allocZip(n);
  MTab *mtab = allocArray_uint8(n * n);
  char *pstring = malloc(6 * mtab->size + 10);

  // --------------------------------------
  initMTab(mtab, zip, 0);
  initIOrd(zip);
  Group *group = 0;
  Array_uint16 *set = allocArray3_uint16(6,0xffff,0xffff);
  bool notDone = 1;
  while(notDone) {
    notDone = doStep(mtab, zip);
    if(isComplete(mtab, n)) {
      group = createFromGen(n, mtab);
      break;
    }
  }
  // ---------------------------------------
  Group *subgroup = generateSubgroup(group, set);
  printGroup(group);
  printGroup(subgroup);
  uint32_t i;
  for(i = 0; i < n; i++) {
    printf("Order of %u = %u | Invs  of %u = %u\n", i,
           elementOrderi(group, i),
           i, invi(group, i));
  }
  printArray_uint16(pstring, group->invs);
  printArray_uint16(pstring, subgroup->set);
  // ---------------------------------------
  printf("-----------------\n");
  Array_uint16 *perm = allocArray_uint16(n);
  Array_uint16 *util1 = allocArray_uint16(n);
  Array_uint16 *util2 = allocArray_uint16(n);
  Array_uint16 *res = allocArray_uint16(n);
  initBinom(perm, 1);
  bool permPossible = 1;
  while(permPossible) {
    permPossible = minGeneratingSet_noalloc(group, res, perm, util1, util2);
    printf("Set i = %u\n", i);
    printArray_uint16(pstring, res);
    printf("perm = %u  ", permPossible);
    printArray_uint16(pstring, perm);
  }
  freeArray_uint16(util1);
  freeArray_uint16(util2);
  freeArray_uint16(res);
  initBinom(perm, 8);
  bool a = shiftBinom(perm, 7);
  printf("A = %u\n", a);
  freeArray_uint16(perm);
  // ---------------------------------------
  freeGroup(subgroup);
  freeArray_uint16(set);
  freeGroup(group);
  free(pstring);
  freeArray_uint8(mtab);
  freeZip(zip);
}

void example_2() {
  uint32_t n = 8;
  char *pstring = malloc(1000);
  Map_uint16 *f = allocMap_uint16(n, 0);
  uint32_t i;
  for(i = 0; i < n; i++) {
    *at_uint16(f->domain, i) = n - i - 1;
    *at_uint16(f->codomain, i) = i;
  }
  printArray_uint16(pstring, f->domain);
  printArray_uint16(pstring, f->codomain);
  //printf("Map is valid %u\n", isValidMap(f));
  printf("Maps are composable %u\n", areComposableMaps(f, f));
  Map_uint16 *ff = compMaps_alloc(f, f);
  printArray_uint16(pstring, ff->domain);
  printArray_uint16(pstring, ff->codomain);
  printf("f vs f %u\n", areEqualMaps(f, f));
  printf("f vs ff %u\n", areEqualMaps(f, ff));
  freeMap_uint16(ff);
  freeMap_uint16(f);
  free(pstring);
}

void example_3() {
  char *pstring = malloc(1000);
  Array_uint16 *array = allocArray_uint16(4);
  initPerm(array);
  bool permRes = 1;
  while(permRes == 1) {
    printArray_uint16(pstring, array);
    permRes = shiftPerm(array);
  }
  freeArray_uint16(array);
  free(pstring);
}

void example_copy() {
  char *pstring = malloc(1000);
  Array_uint16 *array = allocArray_uint16(9);
  initPerm(array);
  Array_uint16 *copy = copyArray_uint16(array);
  printArray_uint16(pstring, copy);
  freeArray_uint16(array);
  freeArray_uint16(copy);
  free(pstring);
}

void example_sn() {
  Group *sn = createSn(7);
  printf("Is valid group: %u\n", isValid(sn));
  freeGroup(sn);
}

int main() {
  //example_1();
  //example_2();
  //example_3();
  //example_copy();
  example_sn();
}

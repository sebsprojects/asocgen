#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gen/asocgen.h"
#include "group/group_includes.h"


int main() {
  uint8_t n = 9;
  Zip *zip = allocZip(n);
  MTab *mtab = allocArray_uint8(n * n);
  char *pstring = malloc(6 * mtab->size + 10);

  // --------------------------------------
  initMTab(mtab, zip, 0);
  initIOrd(zip);
  Group *group = 0;
  Array_uint16 *set = allocArray2_uint16(0, 0xffff);
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
    printf("Order of %u = %u | Invs  of %u = %u\n", i, elementOrderi(group, i),
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
  initPerm(perm, 1);
  bool permPossible = 1;
  while(permPossible) {
    permPossible = minGeneratingSet_noalloc(group, res, perm, util1, util2);
    //printf("Set i = %u\n", i);
    //printArray_uint16(pstring, res);
    printf("perm = %u  ", permPossible);
    printArray_uint16(pstring, perm);
  }
  freeArray_uint16(util1);
  freeArray_uint16(util2);
  freeArray_uint16(res);
  initPerm(perm, 8);
  bool a = shiftPerm(perm, 7);
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

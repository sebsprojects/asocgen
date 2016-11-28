#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gen/asocgen.h"
#include "group/group_includes.h"


int main() {
  uint8_t n = 8;
  Zip *zip = allocZip(n);
  MTab *mtab = allocArray_uint8(n * n);
  char *pstring = malloc(6 * mtab->size + 10);

  initMTab(mtab, zip, 0);
  initIOrd(zip);

  Group *group = 0;
  Array_uint16 *set = allocArray2_uint16(0, 6);
  bool notDone = 1;
  while(notDone) {
    notDone = doStep(mtab, zip);
    if(isComplete(mtab, n)) {
      group = createFromGen(n, mtab);
      break;
    }
  }
  Group *subgroup = generateFrom_togroup(group, set);
  printGroup(group);
  printGroup(subgroup);
  uint32_t i;
  for(i = 0; i < n; i++) {
    printf("Order of %u = %u | Invs  of %u = %u\n", i, elementOrder(group, i),
           i, compInv(group, i));
  }
  printArray_uint16(pstring, group->invs);

  freeGroup(subgroup);
  freeArray_uint16(set);
  freeGroup(group);
  free(pstring);
  freeArray_uint8(mtab);
  freeZip(zip);
}

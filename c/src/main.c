#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gen/asocgen.h"
#include "group/group_common.h"


int main() {
  uint8_t n = 6;
  Zip *zip = allocZip(n);
  MTab *mtab = allocArray_uint8(n * n);
  char *pstring = malloc(6 * mtab->size + 10);

  initMTab(mtab, zip, 0);
  initIOrd(zip);

  Group *group = 0;
  bool notDone = 1;
  while(notDone) {
    notDone = doStep(mtab, zip);
    if(isComplete(mtab, n)) {
      group = createFromGen(n, mtab);
      break;
    }
  }

  printGroup(group);
  uint32_t i;
  for(i = 0; i < n; i++) {
    printf("Order of %u = %u\n", i, elementOrder(group, i));
  }

  freeGroup(group);
  free(pstring);
  freeArray_uint8(mtab);
  freeZip(zip);
}

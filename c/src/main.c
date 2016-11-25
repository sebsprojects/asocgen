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

  Group *group = allocGroup(n);
  createCn(group);
  printGroup(group);
  freeGroup(group);

  /*
  bool notDone = 1;
  while(notDone) {
    notDone = doStep(mtab, zip);
  }
  printf("Total Number of Groups: %u\n", zip->numGroups);
  */

  free(pstring);
  freeArray_uint8(mtab);
  freeZip(zip);
}

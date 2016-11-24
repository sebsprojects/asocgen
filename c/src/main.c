#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gen/asocgen.h"


int main() {
  uint8_t n = 8;
  Zip zipv = allocZip(n);
  Info infov; infov.numGroups = 0;
  Info *info = &infov;
  Zip *zip = &zipv;
  MTab *mtab = allocArray_uint8(n * n);
  char *pstring = malloc(6 * mtab->size + 10);

  initMTab(mtab, zip, 0);
  initIOrd(zip);

  //printMTab(pstring, mtab, n);
  //printZip(zip);
  //printIOrd(pstring, zip);

  bool notDone = 1;
  while(notDone) {
    //printMTab(pstring, mtab, n);
    notDone = doStep(mtab, zip, info);
  }

  printf("Total Number of Groups: %u\n", info->numGroups);

  free(pstring);
  freeArray_uint8(mtab);
  freeZip(zip);
}

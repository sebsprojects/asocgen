#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gen/asocgen.h"
#include "common/common_includes.h"
#include "group/group_includes.h"

void example_hom() {
  uint32_t n = 200;
  Group *cn = createCn(n);
  Group *c2n = createCn(2 * n);
  group_printSummary(cn);
  group_printSummary(c2n);
  Map_uint16 *map = allocMap_uint16(2 * n, 1);
  uint32_t i;
  for(i = 0; i < 2 * n; i++) {
    *at_uint16(map->domain, i) = i;
    *at_uint16(map->codomain, i) = i % 2;
  }
  printf("is valid map: %u\n", isValidMap(map));
  GroupHom *hom = allocGroupHom(c2n, cn, map);
  printf("is valid hom: %u\n", isValidGroupHom(hom));
  freeGroupHom(hom);
  freeMap_uint16(map);
  group_free(c2n);
  group_free(cn);
}

int main() {
  example_hom();
}

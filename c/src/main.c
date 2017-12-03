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
  Map_uint16 *map = mapui16_alloc(2 * n, 1);
  uint32_t i;
  for(i = 0; i < 2 * n; i++) {
    *aui16_at(map->domain, i) = i;
    *aui16_at(map->codomain, i) = i % 2;
  }
  printf("is valid map: %u\n", mapui16_isValid(map));
  GroupHom *hom = allocGroupHom(c2n, cn, map);
  printf("is valid hom: %u\n", isValidGroupHom(hom));
  freeGroupHom(hom);
  mapui16_free(map);
  group_free(c2n);
  group_free(cn);
}

void example_sn() {
  Group *s5 = createSn(5);
  group_printSummary(s5);
  char *pstring = malloc(1000);
  Array_uint16 *set = minGeneratingSet_alloc(s5);
  aui16_print(pstring, set);
  aui16_free(set);
  group_free(s5);
  free(pstring);
}

int main() {
  //example_hom();
  example_sn();
}

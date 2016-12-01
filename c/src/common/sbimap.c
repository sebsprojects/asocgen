#include "sbimap.h"


BiMap_uint16 *allocBiMap_uint16(Array_uint16 *co) {
  BiMap_uint16 *bimap = malloc(sizeof(BiMap_uint16));
  bimap->codomain = co;
  uint32_t i;
  uint16_t ele;
  uint16_t maxEle = 0;
  for(i = 0; i < co->size; i++) {
    ele = *at_uint16(co, i);
    if(ele > maxEle) maxEle = ele;
  }
  bimap->domain = allocArray_uint16(maxEle + 1);
  fillArray_uint16(bimap->domain, 0xffff);
  for(i = 0; i < co->size; i++) {
    *at_uint16(bimap->domain, *at_uint16(co, i)) = i;
  }
  return bimap;
}

void freeBiMap_uint16(BiMap_uint16 *bimap) {
  free(bimap->domain);
  free(bimap);
}

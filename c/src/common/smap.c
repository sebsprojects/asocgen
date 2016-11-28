#include "smap.h"


Map_uint16 *allocMap_uint16(uint32_t size) {
  return allocArray_uint16(size);
}

Map_uint16 *allocMapId_uint16(uint32_t size) {
  Array_uint16 *map = allocArray_uint16(size);
  uint32_t i;
  for(i = 0; i < size; i++) {
    *at_uint16(map, i) = i;
  }
  return map;
}

void freeMap_uint16(Map_uint16 *map) {
  freeArray_uint16(map);
}

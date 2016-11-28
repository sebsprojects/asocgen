#ifndef SMAP
#define SMAP

#include "sarray.h"


typedef Array_uint16 Map_uint16;

Map_uint16 *allocMap_uint16(uint32_t size);
Map_uint16 *allocMapId_uint16(uint32_t size);

void freeMap_uint16(Map_uint16 *map);

inline uint16_t *mapTo_uint16(Map_uint16 *map, uint16_t index) {
#ifdef BOUNDS_CHECK
  if(index >= map->size) {
    fprintf(stderr, "error: Map_uint16 out of bounds: size=%u, index=%u \n",
            map->size, index);
    exit(1);
  }
#endif
  return at_uint16(map, index);
}

// For now: stupid linear search
inline uint16_t mapFrom_uint16(Map_uint16 *map, uint16_t ele) {
  uint32_t i;
  for(i = 0; i < map->size; i++) {
    if(*at_uint16(map, i) == ele) return i;
  }
#ifdef BOUNDS_CHECK
  fprintf(stderr, "error: Map_uint16 mapFrom: no preimage to %u\n", ele);
  exit(1);
#endif
  return 0xffff;
}


#endif

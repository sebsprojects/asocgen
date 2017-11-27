#include "smap.h"


uint16_t mapEle_uint16(Map_uint16 *map, uint16_t ele) {
  uint32_t index;
  if(map->indexed) {
    index = ele;
  } else {
    index = indexof_uint16(map->domain, ele);
  }
  return *at_uint16(map->codomain, index);
}

uint16_t mapInd_uint16(Map_uint16 *map, uint32_t ind) {
  return *at_uint16(map->codomain, ind);
}

bool areComposableMaps(Map_uint16 *f, Map_uint16 *g) {
  uint32_t i;
  for(i = 0; i < g->codomain->size; i++) {
    if(indexof_uint16(f->domain, *at_uint16(g->codomain, i)) == 0xffff) {
      return 0;
    }
  }
  return 1;
}

bool areEqualMaps(Map_uint16 *f, Map_uint16 *g) {
  if(f->domain->size != g->domain->size) {
    return 0;
  }
  if(f->indexed && g->indexed) {
    return areEqualArrays_uint16(f->codomain, g->codomain);
  }
  uint32_t i, ind;
  for(i = 0; i < f->domain->size; i++) {
    ind = indexof_uint16(g->domain, *at_uint16(f->domain, i));
    if(ind == 0xffff) { // ele does not exist in domain of g
      return 0;
    }
    if(mapInd_uint16(f, i) != mapInd_uint16(g, ind)) { // f(ele) != g(ele)
      return 0;
    }
  }
  return 1;
}

void compMaps_noalloc(Map_uint16 *f, Map_uint16 *g, Map_uint16 *comp,
                      bool hasDomainSet) {
  uint32_t i;
  for(i = 0; i < g->domain->size; i++) {
    if(!hasDomainSet) {
      *at_uint16(comp->domain, i) = *at_uint16(g->domain, i);
    }
    *at_uint16(comp->codomain, i) = mapEle_uint16(f, mapInd_uint16(g, i));
  }
}

Map_uint16 *compMaps_alloc(Map_uint16 *f, Map_uint16 *g) {
  Map_uint16 *comp = allocMap_uint16(g->domain->size, g->indexed);
  compMaps_noalloc(f, g, comp, 0);
  return comp;
}

Map_uint16 *allocMap_uint16(uint32_t size, bool indexed) {
  Map_uint16 *map = malloc(sizeof(Map_uint16));
  map->indexed = indexed;
  map->domain = allocArray_uint16(size);
  map->codomain = allocArray_uint16(size);
  return map;
}

Map_uint16 *allocMap_ref_uint16(uint32_t size, bool indexed,
                                Array_uint16 *domain,
                                Array_uint16 *codomain) {
  Map_uint16 *map = malloc(sizeof(Map_uint16));
  map->indexed = indexed;
  map->domain = domain;
  map->codomain = codomain;
  return map;
}

void freeMap_ref_uint16(Map_uint16 *map) {
  free(map);
}

void freeMap_uint16(Map_uint16 *map) {
  freeArray_uint16(map->codomain);
  freeArray_uint16(map->domain);
  free(map);
}

void toId(Map_uint16 *map) {
  map->indexed = 1;
  uint16_t i;
  for(i = 0; i < map->domain->size; i++) {
    *at_uint16(map->domain, i) = i;
    *at_uint16(map->codomain, i) = i;
  }
}

bool isValidMap(Map_uint16 *map) {
  uint32_t i, j;
  uint32_t n = map->domain->size;
  if(n != map->codomain->size) {
    return 0;
  }
  for(i = 0; i < n; i++) {
    for(j = 0; j < i; j++) {
      if(*at_uint16(map->domain, i) == *at_uint16(map->domain, j)) {
        return 0;
      }
    }
    if(*at_uint16(map->codomain, i) == 0xffff ||
       *at_uint16(map->domain, i) == 0xffff) {
      return 0;
    }
  }
  return 1;
}

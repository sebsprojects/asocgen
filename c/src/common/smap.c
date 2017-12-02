#include "smap.h"

Map_uint16 *mapui16_alloc(uint32_t size, bool indexed) {
  Map_uint16 *map = malloc(sizeof(Map_uint16));
  map->indexed = indexed;
  map->domain = aui16_alloc(size);
  map->codomain = aui16_alloc(size);
  return map;
}

Map_uint16 *mapui16_alloc_ref(uint32_t size, bool indexed,
                              Array_uint16 *domain,
                              Array_uint16 *codomain) {
  Map_uint16 *map = malloc(sizeof(Map_uint16));
  map->indexed = indexed;
  map->domain = domain;
  map->codomain = codomain;
  return map;
}

void mapui16_free(Map_uint16 *map) {
  aui16_free(map->codomain);
  aui16_free(map->domain);
  free(map);
}

void mapui16_free_ref(Map_uint16 *map) {
  free(map);
}

// -------------------------------------------------------------------------

void mapui16_toId(Map_uint16 *map) {
  map->indexed = 1;
  uint16_t i;
  for(i = 0; i < map->domain->size; i++) {
    *aui16_at(map->domain, i) = i;
    *aui16_at(map->codomain, i) = i;
  }
}

bool mapui16_isValid(Map_uint16 *map) {
  uint32_t i, j;
  uint32_t n = map->domain->size;
  if(n != map->codomain->size) {
    return 0;
  }
  for(i = 0; i < n; i++) {
    for(j = 0; j < i; j++) {
      if(*aui16_at(map->domain, i) == *aui16_at(map->domain, j)) {
        return 0;
      }
    }
    if(*aui16_at(map->codomain, i) == 0xffff ||
       *aui16_at(map->domain, i) == 0xffff) {
      return 0;
    }
  }
  return 1;
}

bool mapui16_areEqual(Map_uint16 *f, Map_uint16 *g) {
  if(f->domain->size != g->domain->size) {
    return 0;
  }
  if(f->indexed && g->indexed) {
    return aui16_areEqualArrays(f->codomain, g->codomain);
  }
  uint32_t i, ind;
  for(i = 0; i < f->domain->size; i++) {
    ind = aui16_indexOf(g->domain, *aui16_at(f->domain, i));
    if(ind == 0xffff) { // ele does not exist in domain of g
      return 0;
    }
    if(mapui16_mapInd(f, i) != mapui16_mapInd(g, ind)) { // f(ele) != g(ele)
      return 0;
    }
  }
  return 1;
}

bool mapui16_areComposable(Map_uint16 *f, Map_uint16 *g) {
  uint32_t i;
  for(i = 0; i < g->codomain->size; i++) {
    if(aui16_indexOf(f->domain, *aui16_at(g->codomain, i)) == 0xffff) {
      return 0;
    }
  }
  return 1;
}

bool mapui16_isSurjectiveIn(Map_uint16 *map, Array_uint16 *set) {
  return aui16_isSubset(set, map->codomain);
}

bool mapui16_isInjective(Map_uint16 *map) {
  return !aui16_hasDuplicates(map->codomain);
}

void mapui16_comp_noalloc(Map_uint16 *f, Map_uint16 *g, Map_uint16 *comp,
                      bool hasDomainSet) {
  uint32_t i;
  for(i = 0; i < g->domain->size; i++) {
    if(!hasDomainSet) {
      *aui16_at(comp->domain, i) = *aui16_at(g->domain, i);
    }
    *aui16_at(comp->codomain, i) = mapui16_mapEle(f, mapui16_mapInd(g, i));
  }
}

Map_uint16 *mapui16_comp_alloc(Map_uint16 *f, Map_uint16 *g) {
  Map_uint16 *comp = mapui16_alloc(g->domain->size, g->indexed);
  mapui16_comp_noalloc(f, g, comp, 0);
  return comp;
}

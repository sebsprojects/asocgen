#ifndef SBIMAP
#define SBIMAP

#include "common.h"
#include "sarray16.h"

/*
  Representation of a discrete map. Even if the map is not 1:1, always an
  element of index i in the domain maps to codomain[i].

  For a map to be valid it is required that
    domain->size == codomain->size
    domain contains no duplicates, no 0xffff
    codomain contains no 0xffff
*/

struct Map_uint16 {
  bool indexed;
  Array_uint16 *domain;
  Array_uint16 *codomain;
};
typedef struct Map_uint16 Map_uint16;


// --------------------------------------------------------------------------
// Alloc / Free
// --------------------------------------------------------------------------

Map_uint16 *mapui16_alloc(uint32_t size, bool indexed);
Map_uint16 *mapui16_alloc_ref(uint32_t size, bool indexed,
                              Array_uint16 *domain, Array_uint16 *codomain);
void mapui16_free(Map_uint16 *map);
void mapui16_free_ref(Map_uint16 *map);


// --------------------------------------------------------------------------
// MapU
// --------------------------------------------------------------------------

void mapui16_toId(Map_uint16 *map);

bool mapui16_isValid(Map_uint16 *map);

bool mapui16_areEqual(Map_uint16 *f, Map_uint16 *g);

/*
 * For maps to be composable it is required that
 *   g->codomain is subset of f->domain
 */
bool mapui16_areComposable(Map_uint16 *f, Map_uint16 *g);

bool mapui16_isSurjectiveIn(Map_uint16 *map, Array_uint16 *set);
bool mapui16_isInjective(Map_uint16 *map);

/*
 * g->domain->size must be equal to comp->domain->size to get a valid map
 */
void mapui16_comp_noalloc(Map_uint16 *f, Map_uint16 *g, Map_uint16 *comp,
                          bool hasDomainSet);

Map_uint16 *mapui16_comp_alloc(Map_uint16 *f, Map_uint16 *g);


// --------------------------------------------------------------------------
// Inline Functions
// --------------------------------------------------------------------------

inline uint16_t mapui16_mapEle(Map_uint16 *map, uint16_t ele) {
  uint32_t index;
  if(map->indexed) {
    index = ele;
  } else {
    index = aui16_indexOf(map->domain, ele);
  }
  return *aui16_at(map->codomain, index);
}

inline uint16_t mapui16_mapInd(Map_uint16 *map, uint32_t ind) {
  return *aui16_at(map->codomain, ind);
}

#endif

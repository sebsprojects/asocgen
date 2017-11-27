#ifndef SBIMAP
#define SBIMAP

#include "common.h"
#include "sarray.h"

/*
 * Representation of a discrete map. Even if the map is not 1:1, always an
 * element of index i in the domain maps to codomain[i].
 *
 * For a map to be valid it is required that
 *   domain->size == codomain->size
 *   domain contains no duplicates, no 0xffff
 *   codomain contains no 0xffff
 */
struct Map_uint16 {
  bool indexed;
  Array_uint16 *domain;
  Array_uint16 *codomain;
};
typedef struct Map_uint16 Map_uint16;

bool isValidMap(Map_uint16 *map);

Map_uint16 *allocMap_uint16(uint32_t size, bool indexed);
Map_uint16 *allocMap_ref_uint16(uint32_t size, bool indexed,
                                Array_uint16 *domain, Array_uint16 *codomain);
void freeMap_uint16(Map_uint16 *map);
void freeMap_ref_uint16(Map_uint16 *map);

void toId(Map_uint16 *map);

// Ele -> Ele
uint16_t mapEle_uint16(Map_uint16 *map, uint16_t ele);

// Ind -> Ele
uint16_t mapInd_uint16(Map_uint16 *map, uint32_t ind);

/*
 * For maps to be composable it is required that
 *   g->codomain is subset of f->domain
 */
bool areComposableMaps(Map_uint16 *f, Map_uint16 *g);

bool areEqualMaps(Map_uint16 *f, Map_uint16 *g);


/*
 * g->domain->size must be equal to comp->domain->size to get a valid map
 */
void compMaps_noalloc(Map_uint16 *f, Map_uint16 *g, Map_uint16 *comp,
                      bool hasDomainSet);
Map_uint16 *compMaps_alloc(Map_uint16 *f, Map_uint16 *g);


#endif

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

/*
  A notfixed point is a point with x != f(x)
 */
bool mapui16_hasNotfixedPoints(Map_uint16 *map);

/*
  If there are no notfixed points (i.e. map is id) then this returns 0 which
  is a wrong result. Always call hasNotfixedPoint beforehand to make sure.
 */
uint16_t mapui16_getMaximalNotfixedImage(Map_uint16 *map);


// --------------------------------------------------------------------------
// Printing
// --------------------------------------------------------------------------

/*
  For doc see sarray.h. The formatting looks like this:

  <   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,
  < 123,  44,   5,   0, 123,  34,  42,  44, 432,   1,   0,

     12,  13,  14,  15                                     >
      0, 132,  42,  43                                     >

  Note that the padding should be the maximum of pad for the domain / codomain
  range to have it nicely aligned.
 */
void mapui16_sprintLine(Map_uint16 *map, char *pstring, uint32_t indFrom,
                        uint32_t indTo, uint32_t indent, uint32_t digitPad);

void mapui16_sprintToNum(Map_uint16 *map, char *pstring,
                         uint32_t elePerLine, uint32_t indent);
void mapui16_sprintToWidth(Map_uint16 *map, char *pstring,
                           uint32_t width, uint32_t indent);
void mapui16_sprintDefault(Map_uint16 *map, char *pstring);

void mapui16_printToNum(Map_uint16 *map, uint32_t elePerLine,uint32_t indent);
void mapui16_printToWidth(Map_uint16 *map, uint32_t width, uint32_t indent);
void mapui16_printDefault(Map_uint16 *map);

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

/*
  Maps from index to element
 */
inline uint16_t mapui16_mapInd(Map_uint16 *map, uint32_t ind) {
  return *aui16_at(map->codomain, ind);
}

#endif

#ifndef SBIMAP
#define SBIMAP

#include "sarray.h"


struct BiMap_uint16 {
  Array_uint16 *codomain;
  Array_uint16 *domain;
};
typedef struct BiMap_uint16 BiMap_uint16;

BiMap_uint16 *allocBiMap_uint16(Array_uint16 *co);
void freeBiMap_uint16(BiMap_uint16 *bimap);

inline uint16_t *bimapTo_uint16(BiMap_uint16 *bimap, uint16_t index) {
#ifdef BOUNDS_CHECK
  if(index >= bimap->codomain->size) {
    fprintf(stderr,
            "error: BiMap_uint16 mapTo out of bounds: size=%u, index=%u \n",
            bimap->codomain->size, index);
    exit(1);
  }
#endif
  return at_uint16(bimap->codomain, index);
}

inline uint16_t *bimapFrom_uint16(BiMap_uint16 *bimap, uint16_t index) {
#ifdef BOUNDS_CHECK
  if(index >= bimap->domain->size) {
    fprintf(stderr,
            "error: BiMap_uint16 mapFrom out of bounds: size=%u, index=%u \n",
            bimap->domain->size, index);
    exit(1);
  }
#endif
  return at_uint16(bimap->domain, index);
}

#endif

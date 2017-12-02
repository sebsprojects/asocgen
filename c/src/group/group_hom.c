#include "group_hom.h"


GroupHom *allocGroupHom(Group *from, Group *to, Map_uint16 *map) {
  GroupHom *hom = malloc(sizeof(GroupHom));
  hom->from = from;
  hom->to = to;
  hom->map = map;
  return hom;
}

void freeGroupHom(GroupHom *hom) {
  free(hom);
}

bool isValidGroupHom(GroupHom *hom) {
  //if(!isValidMap(hom->map)) {
  //  return 0;
  //}
  if(!areEqualSets_uint16(hom->map->domain, hom->from->set)) {
    return 0;
  }
  if(!isSubset_uint16(hom->map->codomain, hom->to->set)) {
    return 0;
  }
  if(!hasGroupHomProp(hom)) {
    return 0;
  }
  return 1;
}

bool hasGroupHomProp(GroupHom *hom) {
  uint32_t n = hom->map->domain->size;
  uint32_t i, j;
  uint16_t a, b, x, y;
  for(i = 0; i < n; i++) {
    a = *at_uint16(hom->map->domain, i);
    for(j = 0; j < n; j++) {
      b = *at_uint16(hom->map->domain, j);
      // could be gopi instead if arrayEqual(hom->map->domain, home->from->set)
      x = mapEle_uint16(hom->map, gop(hom->from, a, b));
      y = gop(hom->to, mapInd_uint16(hom->map, i), mapInd_uint16(hom->map, j));
      if(x != y) {
        return 0;
      }
    }
  }
  return 1;
}

bool hasGroupHomPropFromGen(GroupHom *hom,
                            Array_uint16 *genFrom,
                            Array_uint16* genTo) {
  uint32_t i, j;
  uint16_t a, b, x, y;
  for(i = 0; i < genFrom->size; i++) {
    a = *at_uint16(genFrom, i);
    for(j = 0; j < genFrom->size; j++) {
      b = *at_uint16(genTo, j);
      x = mapEle_uint16(hom->map, gop(hom->from, a, b));
      y = gop(hom->to, mapEle_uint16(hom->map, a), mapEle_uint16(hom->map, b));
      if(x != y) {
        return 0;
      }
    }
  }
  return 1;
}

bool isIsomorphism(GroupHom *hom) {
  return group_order(hom->from) == group_order(hom->to) &&
    isInjective(hom->map);
}

bool isIsomorphismFromGen(GroupHom *hom,
                          Array_uint16 *genFrom,
                          Array_uint16 *genTo) {
  return genFrom->size == genTo->size &&
    hasGroupHomPropFromGen(hom, genFrom, genTo);
}

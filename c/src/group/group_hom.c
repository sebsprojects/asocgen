#include "group_hom.h"

GroupHom *group_allocHom_ref(Group *from, Group *to, Map_uint16 *map) {
  GroupHom *hom = malloc(sizeof(GroupHom));
  hom->from = from;
  hom->to = to;
  hom->map = map;
  return hom;
}

void group_freeHom_ref(GroupHom *hom) {
  free(hom);
}

bool group_isValidHom(GroupHom *hom) {
  //if(!isValidMap(hom->map)) {
  //  return 0;
  //}
  if(!aui16_areEqualSets(hom->map->domain, hom->from->set)) {
    return 0;
  }
  if(!aui16_isSubset(hom->map->codomain, hom->to->set)) {
    return 0;
  }
  if(!group_hasHomProp(hom)) {
    return 0;
  }
  return 1;
}

bool group_hasHomProp(GroupHom *hom) {
  uint32_t n = hom->map->domain->size;
  uint32_t i, j;
  uint16_t a, b, x, y;
  for(i = 0; i < n; i++) {
    a = *aui16_at(hom->map->domain, i);
    for(j = 0; j < n; j++) {
      b = *aui16_at(hom->map->domain, j);
      // could be gopi instead if arrayEqual(hom->map->domain,home->from->set)
      x = mapui16_mapEle(hom->map, gop(hom->from, a, b));
      y = gop(hom->to, mapui16_mapInd(hom->map, i),
              mapui16_mapInd(hom->map, j));
      if(x != y) {
        return 0;
      }
    }
  }
  return 1;
}

bool group_hasHomPropFromGen(GroupHom *hom,
                             Array_uint16 *genFrom,
                             Array_uint16* genTo) {
  uint32_t i, j;
  uint16_t a, b, x, y;
  for(i = 0; i < genFrom->size; i++) {
    a = *aui16_at(genFrom, i);
    for(j = 0; j < genFrom->size; j++) {
      b = *aui16_at(genTo, j);
      x = mapui16_mapEle(hom->map, gop(hom->from, a, b));
      y = gop(hom->to, mapui16_mapEle(hom->map, a),
              mapui16_mapEle(hom->map, b));
      if(x != y) {
        return 0;
      }
    }
  }
  return 1;
}

bool group_isIsomorphism(GroupHom *hom) {
  return group_order(hom->from) == group_order(hom->to) &&
    mapui16_isInjective(hom->map);
}

bool group_isIsomorphismFromGen(GroupHom *hom,
                          Array_uint16 *genFrom,
                          Array_uint16 *genTo) {
  return genFrom->size == genTo->size && // is covered by below but for effic
    group_order(hom->from) == group_order(hom->to) &&
    group_hasHomPropFromGen(hom, genFrom, genTo);
}

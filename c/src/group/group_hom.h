#ifndef GROUPHOM
#define GROUPHOM

#include "../common/common_includes.h"
#include "../group/group.h"

struct GroupHom {
  Group *from;
  Group *to;
  Map_uint16 *map;
};
typedef struct GroupHom GroupHom;

/*
  For a valid hom, it is required that from->set is (content, order) equal to
  map->domain and map->codomain is a subset of to->set.
*/
GroupHom *allocGroupHom(Group *from, Group *to, Map_uint16 *map);
void freeGroupHom(GroupHom *hom);

/*
  Assumes valid groups and valid map
*/
bool isValidGroupHom(GroupHom *hom);

/*
  Assumes valid groups
*/
bool hasGroupHomProp(GroupHom *hom);
bool hasGroupHomPropFromGen(GroupHom *hom,
                            Array_uint16 *genFrom,
                            Array_uint16* genTo);

/*
  Assumes valid homomorphism
*/
bool isIsomorphism(GroupHom *hom);
bool isIsomorphismFromGen(GroupHom *hom,
                          Array_uint16 *genFrom,
                          Array_uint16 *genTo);

#endif

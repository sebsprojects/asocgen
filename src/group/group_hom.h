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


// --------------------------------------------------------------------------
// Management
// --------------------------------------------------------------------------

/*
  For a valid hom, it is required that from->set is (content, order) equal to
  map->domain and map->codomain is a subset of to->set.
*/
GroupHom *group_allocHom_ref(Group *from, Group *to, Map_uint16 *map);
void group_freeHom_ref(GroupHom *hom);


// --------------------------------------------------------------------------
// Basic Functions
// --------------------------------------------------------------------------

/*
  Assumes valid groups and valid map
*/
bool group_isValidHom(GroupHom *hom);

/*
  Assumes valid groups
*/
bool group_hasHomProp(GroupHom *hom);
bool group_hasHomPropFromGen(GroupHom *hom,
                             Array_uint16 *genFrom,
                             Array_uint16* genTo);

/*
  Assumes valid homomorphism
*/
bool group_isIsomorphism(GroupHom *hom);
bool group_isIsomorphismFromGen(GroupHom *hom,
                                Array_uint16 *genFrom,
                                Array_uint16 *genTo);


// --------------------------------------------------------------------------
// Finding Isomorphisms
// --------------------------------------------------------------------------



#endif

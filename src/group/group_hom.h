#ifndef GROUPHOM
#define GROUPHOM

#include <elfc_vecu16.h>
#include <elfc_mapu16.h>

#include "group.h"

/*
 * A homomorphism between from -> to via map
 */
struct GroupHom {
  Group *from;
  Group *to;
  Mapu16 *map;
};
typedef struct GroupHom GroupHom;


// ---------------------------------------------------------------------------
// Management
// ---------------------------------------------------------------------------

GroupHom *group_allocHom_ref(Group *from, Group *to, Mapu16 *map);
void group_freeHom_ref(GroupHom *hom);


// ---------------------------------------------------------------------------
// Information
// ---------------------------------------------------------------------------

/*
 * Assumes that groups and maps are valid as standalone objects.
 * Checks that the map really maps between the groups and has th hom prop
 */
bool group_isValidHom(GroupHom *hom);

/*
 * Let h be the hom. Checks that h(x * y) = h(x) * h(y) holds for all elements
 * x, y in hom->from
 */
bool group_hasHomProp(GroupHom *hom);

/*
 * Checks for equality of group orders, injectivity of hom and isValidHom
 * in this order
 */
bool group_isIsomorphism(GroupHom *hom);


// ---------------------------------------------------------------------------
// Working with partial homomorphisms defined only on generating sets
// ---------------------------------------------------------------------------

/*
 * Checks h(x * y) = h(x) * h(y) only for x, y in a generating set of
 * hom->from. This is sufficient to know that all elements have this property
 *
 * This function does NOT require a valid homomorphism:
 * map->domain may be restricted to
 *   1. all elements of genFrom
 *   2. all possible products of elements from genFrom
 * and may exclude other elements
 *
 * Requires that the map->domain is filled from the beginning and 0xffff
 * appears exclusively as trailing elements
 */
bool group_hasHomPropFromGen(GroupHom *hom, Vecu16 *genFrom);

/*
 * If hom hasHomPropFromGen, then hom->map is extended to the whole
 * groups. This requires that hom->map has sufficient space allocated
 * (in domain and codomain)
 */
void group_completeHomFromGen(GroupHom *hom);

/*
 * Checks all possible partial maps between genFrom and genTo for the
 * homomorphism property. If one is found, the map is extended to the
 * whole hom and true is returned. Otherwise false is returned and hom->map
 * is left invariant
 */
bool group_findHomFromGen(GroupHom *hom, Vecu16 *genFrom, Vecu16 *genTo);

#endif

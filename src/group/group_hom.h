#ifndef GROUPHOM
#define GROUPHOM

#include <elfc_vecu16.h>
#include <elfc_vecptr.h>
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
// Gen
// ---------------------------------------------------------------------------

/*
 * Given a partial hom->map from a (fixed) generating set genSet of hom->from
 * and given a decomposition of all elements from hom->from into products of
 * elements from genSet, expand the map to all elements of
 * hom->from. This may or may not result in a valid homomorphism
 *
 * For speed it is required that hom->map is filled from the beginning with
 * the partial mapping
 */
void group_completeHomFromGen(GroupHom *hom,
                              Vecu16* genSet,
                              Vecptr *genDecompVec);

#endif

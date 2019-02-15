#ifndef GROUPHOM
#define GROUPHOM

#include <elfc_vecu16.h>
#include <elfc_vecptr.h>
#include <elfc_mapu16.h>

#include "group.h"
#include "group_gen.h"

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
 * hom->map is expected to
 *   a) have a complete domain, i.e. hom->map->domain == hom->from->set
 *   b) have a codomain filled with 0xffff expect for
 *   c) images for all elements of genSet
 */
void group_completeMapFromGen(GroupHom *hom,
                              Vecu16 *genSet,
                              Vecptr *genDecompVec);

/*
 * A struct containing necessar vecu16s to execute findIsomorphismFromGen
 * without any allocations. Used to call minGeneratingSetConstr_noalloc
 */
struct HomIsoUtils {
  Vecu16 *genTo;                  // The genSet of hom->to
  Vecu16 *mperm;                  // Perm for mapping genSets onto each other
  Vecu16 *genFromOrders;          // Store the ele orders for genFrom
  Vecptr *genDecompVec;           // Full decomp into elements from genSetFrom
  Vecu16 *genOrderConstr;            // For group_minGeneratingSetConstr
  GenConstrUtils *genConstrUtils; // For group_minGeneratingSetConstr
};
typedef struct HomIsoUtils HomIsoUtils;

HomIsoUtils *group_allocSetupIsoUtils(GroupHom *hom, Vecu16 *genFrom);
void group_freeIsoUtils(HomIsoUtils *isoUtils);

/*
 * This functions takes a incomplete hom (map must be allocated but not set)
 * and checks for isomorphism between hom->from and hom->to
 */
bool group_checkForIsomorphismFromGen(GroupHom *hom,
                                      Vecu16 *genSetFrom,
                                      Vecu16 *binom,
                                      HomIsoUtils *isoUtils);

#endif

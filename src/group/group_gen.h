#ifndef GROUP_GEN
#define GROUP_GEN

#include <elfc_vecptr.h>

#include "group.h"


// --------------------------------------------------------------------------
// Generating from group elements
// --------------------------------------------------------------------------

/*
 * Generates from the elements (not indices) in set. Returns the generated
 * set in res (elements, not indicies, may be intersperesed with 0xffff).

 * Size req : |group| = |res| = |util|, set can be unordered & 0xffff term
 * res and utils will be ovewritten, set not
 */
void group_generateFrom_noalloc(Group *group,
                                Vecu16 *set,   // set to generate from
                                Vecu16 *res,   // result
                                Vecu16 *util); // fills up, unordered


/*
 * Returns a newly allocated array containing the result of generating from set

 * The result is not tightly packed (0xffff possibly intersperesed
 * between values). For a tight packing call truncGeneratedSet afterwards
 */
Vecu16 *group_generateFrom_alloc(Group *group, Vecu16 *set);

/*
 * Returns a decomposition of all elements of the group
 * into a product of elements from a generating set genSet.
 * The result is stored in genDecomp. Indices into this vectptr correspond to
 * indices in group->set. The vecu16 stored in this vecptr need to have a
 * least size [sum over genSet: group_order(ele)].
 * The vector is filled from the
 * beginning. Excessive space is filled 0xffff, the vector size is left
 * invariant.
 * The multiplication of elements is from the right, i.e. a genDecomp
 * [ 1 3 3 1 1 1 ] means ele = 1 * 3 * 3 * 1 * 1 * 1
 * genSet may be 0xffff terminated but must be filled sequentially from the
 * start
 */
void group_genDecomposition(Group *group,
                            Vecu16 *genSet,     // gen set of group
                            Vecptr *genDecomp); // result


// --------------------------------------------------------------------------
// Generating sets of groups
// --------------------------------------------------------------------------

/*
 * Searches for the first generating set with exactly pn elements starting with
 * binom and shifting until either
 *  a) a set is found:
 *    * The permutation now is the configuration after (!) the successful one
 *      if the shift was possible and returns 1. If no shift was possible
 *      returns 0.
 *    * res now contains scattered (non-tight) element (not index) generating
 *      set
 *  b) all binoms were tried (unsuccessfully):
 *    * res is set to 0xffff, returns 0
 *
 * order(group), res, util1, util2 have to have the same size
 * res, util1, util2 will be overwritten, binom will be shifted until success
 * or failure (end position of binom at pn)
 */
bool group_generatingSet(Group *group,
                         Vecu16 *res,   // result
                         Vecu16 *binom, // binom to use
                         Vecu16 *util1, // util
                         Vecu16 *util2, // util for genFrom
                         u32 pn);       // pn-subsets are considered


/*
 * Searches for a minimal generating set starting at the state of binom.
 * Calls generatingSet with increasing pn (as it continues to fail) until
 * a generating set is found. Similar to generatingSet, the binom is shifted
 * one after the successful one (if possible; returns 1) or returns 0 otherwise

 * array sizes have to comply with order(group) and will be overwritten
 */
bool group_minGeneratingSet_noalloc(Group *group,
                                    Vecu16 *res,    // result
                                    Vecu16 *binom,  // binom to use
                                    Vecu16 *util1,  // binom to use
                                    Vecu16 *util2); // util for genFrom

struct GenConstrUtils {
  Vecu16 *groupEleOrders; // stores pre-calculated group element orders
  Vecu16 *orderUtil;      // to compare element orders
  Vecu16 *genUtil1;       // util for generateFrom
  Vecu16 *genUtil2;       // util for generateFrom
};
typedef struct GenConstrUtils GenConstrUtils;

/*
 * Helper for setting up GenOrderConstr
 */
GenConstrUtils *group_allocSetupConstrUtils(Group *group, u32 orderConstrSize);
void group_freeConstrUtils(GenConstrUtils *constrUtils);

/*
 * Searches for a minimal generating set like above, but with additional
 * constraints:
 *   a) Only k-minimal generating sets are considered where k is determined
 *      by the provided binom. Larger generating sets are not considered.
 *      This is in contrast to the normal version above.
 *   b) orderConstr places a constraint on the order of the elements in the
 *      generating set to be returned.
 *
 * > orderConstr must be of equal length as the binom (and therefore the genSet
 *   size). Currently, only a full order-constraint is possible. This
 *   vector must be sorted!
 * > groupEleOrders must be filled with the group element orders complying
 *   with the indexing of group->set.
 * > util must equal size to orderConstr and may be uninitialized.
 *
 * Returns 1 if the binom shift was possible and this function may be called
 * again with the new binom. Return 0 otherwise.
 */
bool group_minGeneratingSetConstr(Group *group,
                                  Vecu16 *res,
                                  Vecu16 *binom,
                                  Vecu16 *orderConstr,
                                  GenConstrUtils *genConstrUtils);

/*
 * Returns a pointer to a newly allocated array containing the first minimal
 * generating set found
 */
Vecu16 *group_minGeneratingSet_alloc(Group *group);


// --------------------------------------------------------------------------
// Generating a subgroup from elements
// --------------------------------------------------------------------------

Group *group_generateSubgroup_alloc(Group *group, Vecu16 *set);


// --------------------------------------------------------------------------
// Utility
// --------------------------------------------------------------------------

/*
 * Packs n elements in res tightly into the first n array spaces filling out
 * any 0xffff entries. The order is preserved.
 * If shirnk is true, it shrinks the array to the appropriate size
 */
void group_truncGeneratedSet(Vecu16 *res, bool shrink);



#endif

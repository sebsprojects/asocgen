#ifndef GROUP_GEN
#define GROUP_GEN

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

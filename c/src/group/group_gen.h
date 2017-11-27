#ifndef GROUP_GEN
#define GROUP_GEN

#include "group.h"
#include "../common/common_includes.h"

/*
  Generates from the elements (not indices) in set. Returns the generated
  set in res (elements, not indicies, may be intersperesed with 0xffff).

  Size req : |group| = |res| = |util|, set can be unordered & 0xffff term
  res and utils will be ovewritten, set not
 */
void generateFrom_noalloc(Group *group,
                          Array_uint16 *set,   // set to generate from
                          Array_uint16 *res,   // result
                          Array_uint16 *util); // fills up, unordered


/*
  Returns a newly allocated array containing the result of generating from set

  The result is not tightly packed (0xffff possibly intersperesed
  between values). For a tight packing truncGeneratedSet is recommended.
 */
Array_uint16 *generateFrom_alloc(Group *group, Array_uint16 *set);


/*
  Searches for the first generating set with exactly pn elements starting with
  perm and shifting until either
    a) a set is found:
      * The permutation now is the configuration after (!) the successful one
        if the shift was possible and returns 1. If no shift was possible
        returns 0.
      * res now contains scattered (non-tight) element (not index) generating
        set
    b) all perms were tried (unsuccessfully):
      * res is set to 0xffff, returns 0

  order(group), res, util1, util2 have to have the same size
  res, util1, util2 will be overwritten, perm will be shifted until success
  or failure (end position of perm at pn)
 */
bool generatingSet(Group *group,
                   Array_uint16 *res,   // result
                   Array_uint16 *perm,  // perm to use
                   Array_uint16 *util1, // util
                   Array_uint16 *util2, // util for genFrom
                   uint32_t pn);        // pn-subsets are considered


/*
  Searches for a minimal generating set starting with the state of perm.
  Calls generatingSet with increasing pn (as it continues to fail) until
  a generating set is found. Similar to generatingSet, the perm is shifted
  one after the successful one (if possible; returns 1) or returns 0 otherwise

  array sizes have to comply with order(group) (except perm) and will be
  overwritten
 */
bool minGeneratingSet_noalloc(Group *group,
                              Array_uint16 *res,    // result
                              Array_uint16 *perm,   // perm to use
                              Array_uint16 *util1,  // perm to use
                              Array_uint16 *util2); // util for genFrom


/*
  Returns a pointer to a newly allocated array containing the first minimal
  generating set found
 */
Array_uint16 *minGeneratingSet_alloc(Group *group);



/*
  Packs n elements in res tightly into the first n array spaces (ordered)
  If shirnk is true, it shrinks the array to the appropriate size

  res has to be ordered in the sense that a1 < a2 < ... < an neglecting
  possible 0xffff entries between ais
 */
void truncGeneratedSet(Group *group, Array_uint16 *res, bool shrink);



#endif

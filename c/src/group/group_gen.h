#ifndef GROUP_GEN
#define GROUP_GEN

#include "group.h"
#include "../common/common.h"


/* Size req : |group| = |res| = |util|, set can be unordered & 0xffff term */
Array_uint16 *generateFrom_alloc(Group *group, Array_uint16 *set);
void generateFrom_noalloc(Group *group,
                          Array_uint16 *set,    // set to generate from
                          Array_uint16 *res,    // result
                          Array_uint16 *util);  // fills up, unordered

Array_uint16 *minGeneratingSet_alloc(Group *group);
void minGeneratingSet_n(Group *group,
                        Array_uint16 *res,   // result
                        Array_uint16 *perm,  // perm to use
                        Array_uint16 *util1, // perm to use
                        Array_uint16 *util2, // util for genFrom
                        uint32_t pn);        // pn-subsets are considered
void minGeneratingSet_noalloc(Group *group,
                              Array_uint16 *res,   // result
                              Array_uint16 *perm,  // perm to use
                              Array_uint16 *util1,  // perm to use
                              Array_uint16 *util2); // util for genFrom

bool shiftPerm(Array_uint16 *perm, uint16_t max);
void initPerm(Array_uint16 *perm, uint16_t n);
void truncGeneratedSet(Group *group, Array_uint16 *res, bool shrink);

#endif

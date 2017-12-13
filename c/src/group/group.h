#ifndef GROUP
#define GROUP

#include "../common/common_includes.h"

/*
  Represents a group. If indexed is 1, then set must be id = { 0, 1, ..., n }
  Invs is a tradeoff between memory usage and comp time
*/
struct Group {
  bool indexed;
  Array_uint16 *set;
  Array_uint16 *gtab;
  Array_uint16 *invs;
};
typedef struct Group Group;


// --------------------------------------------------------------------------
// Management
// --------------------------------------------------------------------------

Group *group_alloc(uint16_t order, bool indexed);
void group_free(Group *group);
void group_setInvs(Group *group);

bool group_checkIndexed(Group *group);

Group *group_getIndexedCopy_alloc(Group *group);
Group *group_getRenamedCopy_alloc(Group *group, Map_uint16 *map);


// --------------------------------------------------------------------------
// Information
// --------------------------------------------------------------------------

bool group_isCommutative(Group *group);
bool group_isCyclic(Group *group);

uint16_t group_elementOrder(Group *group, uint16_t ele);
uint16_t group_elementOrderi(Group *group, uint16_t ind);
Map_uint16 *group_orderDist_alloc(Group *group);

uint16_t group_neutral(Group *group);
uint16_t group_neutrali(Group *group);


// --------------------------------------------------------------------------
// Subgroup related
// --------------------------------------------------------------------------

uint16_t group_conjEle(Group *group, uint16_t toConj, uint16_t a);
uint16_t group_conjElei(Group *group, uint16_t toConj, uint16_t a);

Array_uint16 *group_leftCoset_alloc(Group *group, Group *subgroup,
                                    uint16_t ele);
Array_uint16 *group_rightCoset_alloc(Group *group, Group *subgroup,
                                     uint16_t ele);

bool group_isSubgroup(Group *group, Group *subgroup);
bool group_isNormalSubgroup(Group *group, Group *subgroup);

// --------------------------------------------------------------------------
// Validation
// --------------------------------------------------------------------------

bool group_isValid(Group *group);
bool group_hasValidOp(Group *group);
bool group_isAsoc(Group *group);
bool group_hasNeutral(Group *group);
bool group_hasInvs(Group *group);


// --------------------------------------------------------------------------
// Print
// --------------------------------------------------------------------------

void group_printSummary(Group *group);
void group_print(Group *group);


// --------------------------------------------------------------------------
// Inline Functions
// --------------------------------------------------------------------------

inline uint16_t group_order(Group *group) {
  return group->set->size;
}

inline uint16_t group_inv(Group *group, uint16_t ele) {
  uint16_t ind = ele;
  if(!group->indexed) {
    ind = aui16_indexOf(group->set, ele);
  }
  return *aui16_at(group->invs, ind);
}

inline uint16_t group_invi(Group *group, uint16_t ind) {
  return *aui16_at(group->invs, ind);
}

// ele * ele -> ele
inline uint16_t gop(Group *group, uint16_t i, uint16_t j) {
  if(!group->indexed) {
    i = aui16_indexOf(group->set, i);
    j = aui16_indexOf(group->set, j);
  }
  return *aui16_at(group->gtab, get2DIndex(group_order(group), i, j));
}

// index * index -> index
inline uint16_t gopi(Group *group, uint16_t i, uint16_t j) {
  uint16_t ele = *aui16_at(group->gtab, get2DIndex(group_order(group), i, j));
  if(!group->indexed) {
    return aui16_indexOf(group->set, ele);
  }
  return ele;
}

#endif

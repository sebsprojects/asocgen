#ifndef GROUP
#define GROUP

#include <elfc_common.h>
#include <elfc_vecu16.h>
#include <elfc_mapu16.h>


/*
 * Represents a group. If indexed is 1, then set must be id = { 0, 1, ..., n }
 */
struct Group {
  bool indexed;
  Vecu16 *set;
  Vecu16 *gtab;
};
typedef struct Group Group;


// ---------------------------------------------------------------------------
// Management
// ---------------------------------------------------------------------------

Group *group_alloc(u16 order, bool indexed);
void group_free(Group *group);

bool group_checkIndexed(Group *group);

Group *group_getIndexedCopy_alloc(Group *group);
Group *group_getRenamedCopy_alloc(Group *group, Mapu16 *map);


// ---------------------------------------------------------------------------
// Information
// ---------------------------------------------------------------------------

bool group_isCommutative(Group *group);
bool group_isCyclic(Group *group);

u16 group_elementOrder(Group *group, u16 ele);
u16 group_elementOrderi(Group *group, u16 ind);
Mapu16 *group_orderDist_alloc(Group *group);

u16 group_neutral(Group *group);
u16 group_neutrali(Group *group);

u16 group_inv(Group *group, u16 ele);
u16 group_invi(Group *group, u16 ind);


// ---------------------------------------------------------------------------
// Subgroup related
// ---------------------------------------------------------------------------

u16 group_conjEle(Group *group, u16 toConj, u16 a);
u16 group_conjElei(Group *group, u16 toConj, u16 a);

Vecu16 *group_leftCoset_alloc(Group *group, Group *subgroup, u16 ele);
Vecu16 *group_rightCoset_alloc(Group *group, Group *subgroup, u16 ele);

bool group_isSubgroup(Group *group, Group *subgroup);
bool group_isNormalSubgroup(Group *group, Group *subgroup);


// ---------------------------------------------------------------------------
// Validation
// ---------------------------------------------------------------------------

bool group_isValid(Group *group);

bool group_hasValidSet(Group *group);
bool group_hasValidOp(Group *group);
bool group_isAsoc(Group *group);
bool group_hasNeutral(Group *group);
bool group_hasInvs(Group *group);


// ---------------------------------------------------------------------------
// Print
// ---------------------------------------------------------------------------

void group_printSummary(Group *group);
//void group_print(Group *group);


// ---------------------------------------------------------------------------
// Inline Functions
// ---------------------------------------------------------------------------

inline u16 group_order(Group *group)
{
  return group->set->size;
}

// ele * ele -> ele
inline u16 group_op(Group *group, u16 i, u16 j)
{
  u32 indi = i;
  u32 indj = j;
  if(!group->indexed) {
    vecu16_indexOf(group->set, i, &indi, 0);
    vecu16_indexOf(group->set, j, &indj, 0);
  }
  return *vecu16_at(group->gtab, get2DIndex(group_order(group), indi, indj));
}

// index * index -> index
inline u16 group_opi(Group *group, u16 i, u16 j)
{
  u16 ele = *vecu16_at(group->gtab, get2DIndex(group_order(group), i, j));
  if(!group->indexed) {
    u32 ind = -1;
    bool ok = vecu16_indexOf(group->set, ele, &ind, 0);
#ifdef BOUNDS_CHECK
    if(!ok) {
      // TODO improve this to standardized error-out
    }
#endif
    return ind;
  }
  return ele;
}

#endif

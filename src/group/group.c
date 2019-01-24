#include "group.h"

#include <elfc_math.h>

#include <stdlib.h>
#include <stdio.h>


// ---------------------------------------------------------------------------
// Management
// ---------------------------------------------------------------------------

Group *group_alloc(u16 order, bool indexed)
{
  Group *group = malloc(sizeof(Group));
  group->indexed = indexed;
  group->set = vecu16_alloc(order);
  group->gtab = vecu16_alloc(order * order);
  return group;
}

void group_free(Group *group)
{
  vecu16_free(group->gtab);
  vecu16_free(group->set);
  free(group);
}

bool group_checkIndexed(Group *group)
{
  for(i32 i = 0; i < group_order(group); i++) {
    if(*vecu16_at(group->set, i) != i) {
      return 0;
    }
  }
  return 1;
}

Group *group_getIndexedCopy_alloc(Group *group)
{
  Mapu16 *map = mapu16_alloc(group_order(group), group->indexed);
  vecu16_copyInto(map->domain, group->set);
  vecu16_setToRange(map->codomain, 0, map->codomain->size, 0);
  Group *copy = group_getRenamedCopy_alloc(group, map);
  mapu16_free(map);
  return copy;
}

Group *group_getRenamedCopy_alloc(Group *group, Mapu16 *map)
{
  Group *copy = group_alloc(group_order(group), 0);
  mapu16_mapEleVec(map, group->set, copy->set);
  mapu16_mapEleVec(map, group->gtab, copy->gtab);
  copy->indexed = group_checkIndexed(copy);
  return copy;
}


// ---------------------------------------------------------------------------
// Information
// ---------------------------------------------------------------------------

bool group_isCommutative(Group *group)
{
  u16 n = group_order(group);
  for(i32 i = 0; i < n; i++) {
    for(i32 j = 0; j < n; j++) {
      if(group_opi(group, i, j) != group_opi(group, j, i)) {
        return 0;
      }
    }
  }
  return 1;
}

bool group_isCyclic(Group *group)
{
  u16 n = group_order(group);
  for(i32 i = 0; i < n; i++) {
    if(group_elementOrder(group, *vecu16_at(group->set, i)) == n) {
      return 1;
    }
  }
  return 0;
}

u16 group_elementOrder(Group *group, u16 ele)
{
  u16 r = ele;
  u16 ord = 1;
  u16 neutral = group_neutral(group);
  while(r != neutral) {
    //printf("r = %i\n", r);
    r = group_op(group, r, ele);
    ord++;
  }
  return ord;
}

u16 group_elementOrderi(Group *group, u16 ind)
{
  u16 r = ind;
  u16 ord = 1;
  u16 neutralInd = group_neutrali(group);
  while(r != neutralInd) {
    r = group_opi(group, r, ind);
    ord++;
  }
  return ord;
}

Mapu16 *group_orderDist_alloc(Group *group)
{
  u32 n = group_order(group);
  Vecu16 *primeFac = math_getFactors_alloc(n);
  Vecu16 *orderCounts = vecu16_alloc(primeFac->size);
  vecu16_fill(orderCounts, 0);
  u32 orderi;
  u32 ind = -1;
  for(i32 i = 0; i < n; i++) {
    orderi = group_elementOrderi(group, i);
    if(vecu16_indexOf(primeFac, orderi, &ind, 0)) {
      (*vecu16_at(orderCounts, ind))++;
    }
  }
  // ref alloc is okay since the vectors alloc'd are exactly dom / codom
  Mapu16 *map = mapu16_alloc_ref(primeFac, orderCounts, 0);
  return map;
}

u16 group_neutral(Group *group)
{
  return *vecu16_at(group->set, group_neutrali(group));
}

u16 group_neutrali(Group *group)
{
  for(i32 i = 0; i < group_order(group); i++) {
    if(group_opi(group, i, 0) == 0) {
      return i;
      break;
    }
  }
  return 0xffff; // error value
}


u16 group_invi(Group *group, u16 ind) {
  u32 n = group_order(group);
  u16 neuInd = group_neutrali(group);
  for(i32 i = 0; i < n; i++) {
    if(group_opi(group, ind, i) == neuInd) {
      return i;
    }
  }
  return 0xffff; // might be misleading value. Should produce error value
}

u16 group_inv(Group *group, u16 ele) {
  u32 ind = ele;
  if(!group->indexed) {
    vecu16_indexOf(group->set, ele, &ind, 0);
  }
  return *vecu16_at(group->set, group_invi(group, ind));
}


// ---------------------------------------------------------------------------
// Subgroup related
// ---------------------------------------------------------------------------

u16 group_conjEle(Group* group, u16 toConj, u16 a)
{
  return group_op(group, a, group_op(group, toConj, group_inv(group, a)));
}

u16 group_conjElei(Group *group, u16 toConj, u16 a)
{
  return group_opi(group, a, group_opi(group, toConj, group_invi(group, a)));
}

/*
Vecu16 *group_leftCoset_alloc(Group *group, Group *subgroup, u16 ele)
{
  return 0;
}

Vecu16 *group_rightCoset_alloc(Group *group, Group *subgroup, u16 ele) {
  return 0;
}
*/

bool group_isSubgroup(Group *group, Group *subgroup)
{
  if(!vecu16_isSubset(subgroup->set, group->set)) {
    return 0;
  }
  u16 a, b;
  for(i32 i = 0; i < group_order(subgroup); i++) {
    a = *vecu16_at(subgroup->set, i);
    for(i32 j = 0; j < group_order(subgroup); j++) {
      b = *vecu16_at(subgroup->set, j);
      if(group_op(group, a, b) != group_op(subgroup, a, b)) {
        return 0;
      }
    }
  }
  return 1;
}

bool group_isNormalSubgroup(Group *group, Group *subgroup)
{
  Vecu16 *conjSet = vecu16_alloc(group_order(subgroup));
  bool isNormal = 1;
  for(i32 i = 0; i < group_order(group); i++) {
    // for index i calculate the set of conj elements jij^-1 and check eqlity
    for(i32 j = 0; j < group_order(subgroup); j++) {
      *vecu16_at(conjSet, j) = *vecu16_at(group->set,
                                          group_conjElei(group, j, i));
    }
    isNormal = isNormal && vecu16_areEqualSets(conjSet, subgroup->set);
    if(!isNormal) {
      break;
    }
  }
  vecu16_free(conjSet);
  return isNormal;
}


// ---------------------------------------------------------------------------
// Validation
// ---------------------------------------------------------------------------

bool group_isValid(Group *group)
{
  return group_hasValidSet(group) &&
         group_hasValidOp(group) &&
         group_isAsoc(group) &&
         group_hasNeutral(group) &&
         group_hasInvs(group);
}

bool group_hasValidSet(Group *group)
{
  // if indexed=1 then check if really indexed
  if(group->indexed) {
    for(i32 i = 0; i < group->set->size; i++) {
      if(*vecu16_at(group->set, i) != i) {
        return 0;
      }
    }
  }
  // check that no duplicate entries and no 0xffff entries
  return !vecu16_hasDuplicates(group->set) &&
         !vecu16_contains(group->set, 0xffff, 0);
}

bool group_hasValidOp(Group *group)
{
  u32 n = group_order(group);
  // masks for row, col to check that opi(i, j), opi(j, i) attains all vals
  Vecu16 *row = vecu16_copy(group->set); // vec with no element 0xffff
  Vecu16 *col = vecu16_copy(group->set); // ^
  bool isValid = 1;
  for(i32 i = 0; i < n; i++) {
    for(i32 j = 0; j < n; j++) {
      *vecu16_at(row, group_opi(group, i, j)) = 0xffff;
      *vecu16_at(col, group_opi(group, j, i)) = 0xffff;
    }
    for(i32 j = 0; j < n; j++) {
      // check that only 0xffff entries exist
      isValid = isValid &&
                *vecu16_at(row, j) == 0xffff &&
                *vecu16_at(col, j) == 0xffff;
    }
    if(!isValid) {
      break;
    }
    vecu16_fill(row, 0);
    vecu16_fill(col, 0);
  }
  vecu16_free(row);
  vecu16_free(col);
  return isValid;
}

bool group_isAsoc(Group *group)
{
  u16 ab, ab_c, bc, a_bc;
  u32 n = group_order(group);
  for(u16 a = 0; a < n; a++) {
    for(u16 b = 0; b < n; b++) {
      for(u16 c = 0; c < n; c++) {
        ab = group_opi(group, a, b);
        bc = group_opi(group, b, c);
        ab_c = group_opi(group, ab, c);
        a_bc = group_opi(group, a, bc);
        if(ab_c != a_bc) {
          return 0;
        }
      }
    }
  }
  return 1;
}

bool group_hasNeutral(Group *group)
{
  u32 n = group_order(group);
  u32 neutralInd = group_neutrali(group);
  if(neutralInd >= n) {
    return 0;
  }
  // Check if neutral is indeed neutral (does not check uniqueness)
  for(i32 i = 0; i < n; i++) {
    if(group_opi(group, i, neutralInd) != i ||
       group_opi(group, neutralInd, i) != i) {
      return 0;
    }
  }
  return 1;
}

bool group_hasInvs(Group *group)
{
  u32 n = group_order(group);
  u16 neuInd = group_neutrali(group);
  u32 invInd;
  for(i32 i = 0; i < n; i++) {
    invInd = group_invi(group, i);
    if(invInd >= n) {
      return 0;
    }
    if(group_opi(group, invInd, i) != neuInd ||
       group_opi(group, i, invInd) != neuInd) {
      return 0;
    }
  }
  return 1;
}


// ---------------------------------------------------------------------------
// Print
// ---------------------------------------------------------------------------

/*
void group_print(Group *group) {
  u32 n = group_order(group);
  char *pstring = malloc(n * n * 6 + n * 6 * 2 + 100); // just a guess
  char *valid;
  if(group_isValid(group)) valid = "valid";
  else valid = "INVALID";
  printf("Group of order %u (%s)\n", n, valid);
  vecu16_printSquare(group->gtab, 2);
  free(pstring);
}
*/

void group_printSummary(Group *group) {
  u32 n = group_order(group);
  printf("Summary of group candidate of order %u\n", n);
  printf("  (*) isIndexed   %u\n", group->indexed);
  printf("  (*) hasValidSet %u\n", group_hasValidSet(group));
  printf("  (*) hasGroupOp  %u\n", group_hasValidOp(group));
  printf("  (*) isAssoc     %u\n", group_isAsoc(group));
  printf("  (*) hasNeutral  %u\n", group_hasNeutral(group));
  printf("  (*) hasInvs     %u\n\n", group_hasInvs(group));
  printf("  (*) neutral Element %u\n", group_neutral(group));
  printf("  (*) isCyclic        %u\n", group_isCyclic(group));
  printf("  (*) isCommutative   %u\n\n", group_isCommutative(group));
  //Map_uint16 *orderMap = group_orderDist_alloc(group);
  //printf("  (*) order distribution:\n");
  //mapui16_printToWidth(orderMap, 80, 2);
  //mapui16_free(orderMap);
}

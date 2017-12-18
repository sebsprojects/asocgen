#include "group.h"

#include <stdlib.h>
#include <stdio.h>
#include "group_hom.h"


Group *group_alloc(uint16_t order, bool indexed) {
  Group *group = malloc(sizeof(Group));
  group->indexed = indexed;
  group->set = aui16_alloc(order);
  group->gtab = aui16_alloc(order * order);
  return group;
}

void group_free(Group *group) {
  aui16_free(group->gtab);
  aui16_free(group->set);
  free(group);
}

bool group_checkIndexed(Group *group) {
  uint32_t i;
  for(i = 0; i < group_order(group); i++) {
    if(*aui16_at(group->set, i) != i) {
      return 0;
    }
  }
  return 1;
}

Group *group_getIndexedCopy_alloc(Group *group) {
  Map_uint16 *map = mapui16_alloc(group_order(group), group->indexed);
  aui16_copyInto(map->domain, group->set);
  aui16_setToRange(map->codomain, 0, map->codomain->size, 0);
  Group *copy = group_getRenamedCopy_alloc(group, map);
  mapui16_free(map);
  return copy;
}

Group *group_getRenamedCopy_alloc(Group *group, Map_uint16 *map) {
  Group *copy = group_alloc(group_order(group), 0);
  mapui16_mapArray(map, group->set, copy->set);
  mapui16_mapArray(map, group->gtab, copy->gtab);
  copy->indexed = group_checkIndexed(copy);
  return copy;
}

// --------------------------------------------------------------------------

bool group_isCommutative(Group *group) {
  uint32_t i, j;
  uint16_t n = group_order(group);
  for(i = 0; i < n; i++) {
    for(j = 0; j < n; j++) {
      if(gopi(group, i, j) != gopi(group, j, i)) return 0;
    }
  }
  return 1;
}

bool group_isCyclic(Group *group) {
  uint32_t i;
  uint16_t n = group_order(group);
  for(i = 0; i < n; i++) {
    if(group_elementOrder(group, *aui16_at(group->set, i)) == n) return 1;
  }
  return 0;
}

uint16_t group_elementOrder(Group *group, uint16_t ele) {
  uint16_t r = ele;
  uint16_t ord = 1;
  while(r != 0) {
    r = gop(group, r, ele);
    ord++;
  }
  return ord;
}

uint16_t group_elementOrderi(Group *group, uint16_t ind) {
  uint16_t r = ind;
  uint16_t ord = 1;
  while(r != 0) {
    r = gopi(group, r, ind);
    ord++;
  }
  return ord;
}

Map_uint16 *group_orderDist_alloc(Group *group) {
  uint32_t n = group_order(group);
  Array_uint16 *primeFac = getFactors_alloc(n);
  Array_uint16 *orderCounts = aui16_alloc(primeFac->size);
  aui16_fill(orderCounts, 0);
  uint32_t i, orderi;
  for(i = 0; i < n; i++) {
    orderi = group_elementOrderi(group, i);
    (*aui16_at(orderCounts, aui16_indexOf(primeFac, orderi)))++;
  }
  Map_uint16 *map = mapui16_alloc_ref(primeFac->size, 0,primeFac,orderCounts);
  return map;
}

uint16_t group_neutral(Group *group) {
  return *aui16_at(group->set, group_neutrali(group));
}

uint16_t group_neutrali(Group *group) {
  uint32_t i;
  uint16_t neutralInd = 0xffff;
  for(i = 0; i < group_order(group); i++) {
    if(gopi(group, i, 0) == 0) {
      neutralInd = i;
      break;
    }
  }
  return neutralInd;
}


uint16_t group_invi(Group *group, uint16_t ind) {
  uint32_t n = group_order(group);
  uint16_t neuInd = group_neutrali(group);
  uint32_t i;
  for(i = 0; i < n; i++) {
    if(gopi(group, ind, i) == neuInd) {
      return i;
    }
  }
  return 0xffff;
}

uint16_t group_inv(Group *group, uint16_t ele) {
  uint16_t ind = ele;
  if(!group->indexed) {
    ind = aui16_indexOf(group->set, ele);
  }
  return *aui16_at(group->set, group_invi(group, ind));
}

// --------------------------------------------------------------------------

uint16_t group_conjEle(Group* group, uint16_t toConj, uint16_t a) {
  return gop(group, a, gop(group, toConj, group_inv(group, a)));
}

uint16_t group_conjElei(Group *group, uint16_t toConj, uint16_t a) {
  return gopi(group, a, gopi(group, toConj, group_invi(group, a)));
}

Array_uint16 *group_leftCoset_alloc(Group *group, Group *subgroup,
                                    uint16_t ele) {
  return 0;
}

Array_uint16 *group_rightCoset_alloc(Group *group, Group *subgroup,
                                     uint16_t ele) {
  return 0;
}

bool group_isSubgroup(Group *group, Group *subgroup) {
  if(!aui16_isSubset(subgroup->set, group->set)) {
    return 0;
  }
  uint32_t i, j;
  uint16_t a, b;
  for(i = 0; i < group_order(subgroup); i++) {
    a = *aui16_at(subgroup->set, i);
    for(j = 0; j < group_order(subgroup); j++) {
      b = *aui16_at(subgroup->set, j);
      if(gop(group, a, b) != gop(subgroup, a, b)) {
        return 0;
      }
    }
  }
  return 1;
}

bool group_isNormalSubgroup(Group *group, Group *subgroup) {
  Array_uint16 *conjSet = aui16_alloc(group_order(subgroup));
  uint32_t i, j;
  for(i = 0; i < group_order(group); i++) {
    for(j = 0; j < group_order(subgroup); j++) {
      *aui16_at(conjSet, j) = *aui16_at(group->set,
                                        group_conjElei(group, j, i));
    }
    if(!aui16_areEqualSets(conjSet, subgroup->set)) {
      return 0;
    }
  }
  aui16_free(conjSet);
  return 1;
}

// --------------------------------------------------------------------------

bool group_isValid(Group *group) {
  return group_hasValidOp(group) &&
    group_isAsoc(group) &&
    group_hasNeutral(group) &&
    group_hasInvs(group);
}

bool group_hasValidOp(Group *group) {
  uint32_t n = group_order(group);
  Array_uint16 *row = aui16_copy(group->set);
  Array_uint16 *col = aui16_copy(group->set);
  uint32_t i, j;
  for(i = 0; i < n; i++) {
    for(j = 0; j < n; j++) {
      *aui16_at(row, gopi(group, i, j)) = 0xffff;
      *aui16_at(col, gopi(group, j, i)) = 0xffff;
    }
    for(j = 0; j < n; j++) {
      if(*aui16_at(row, j) != 0xffff || *aui16_at(col, j) != 0xffff) {
        aui16_free(row);
        aui16_free(col);
        return 0;
      }
      *aui16_at(row, j) = 0; // just not 0xffff
      *aui16_at(col, j) = 0; // just not 0xffff
    }
  }
  aui16_free(row);
  aui16_free(col);
  return 1;
}

bool group_isAsoc(Group *group) {
  uint16_t a, b, c, ab, ab_c, bc, a_bc;
  uint32_t n = group_order(group);
  for(a = 0; a < n; a++) {
    for(b = 0; b < n; b++) {
      for(c = 0; c < n; c++) {
	ab = gopi(group, a, b);
	bc = gopi(group, b, c);
        ab_c = gopi(group, ab, c);
	a_bc = gopi(group, a, bc);
	if(ab_c != a_bc) return 0;
      }
    }
  }
  return 1;
}

bool group_hasNeutral(Group *group) {
  uint32_t n = group_order(group);
  uint32_t neutralInd = group_neutrali(group);
  if(neutralInd >= n) {
    return 0;
  }
  // Check if neutral is indeed neutral (does not check uniqueness)
  uint32_t i;
  for(i = 0; i < n; i++) {
    if(gopi(group, i, neutralInd) != i || gopi(group, neutralInd, i) != i) {
      return 0;
    }
  }
  return 1;
}

bool group_hasInvs(Group *group) {
  uint32_t n = group_order(group);
  uint16_t neuInd = group_neutrali(group);
  uint32_t i, invInd;
  for(i = 0; i < n; i++) {
    invInd = group_invi(group, i);
    if(gopi(group, invInd, i) != neuInd || gopi(group, i, invInd) != neuInd) {
      return 0;
    }
  }
  return 1;
}

// --------------------------------------------------------------------------

void group_print(Group *group) {
  uint32_t n = group_order(group);
  char *pstring = malloc(n * n * 6 + n * 6 * 2 + 100); // just a guess
  char *valid;
  if(group_isValid(group)) valid = "valid";
  else valid = "INVALID";
  printf("Group of order %u (%s)\n", n, valid);
  aui16_printSquare(group->gtab, 2);
  free(pstring);
}

void group_printSummary(Group *group) {
  uint32_t n = group_order(group);
  printf("Summary of group candidate of order %u\n", n);
  printf("  (*) hasGroupOp %u\n", group_hasValidOp(group));
  printf("  (*) isAssoc    %u\n", group_isAsoc(group));
  printf("  (*) hasNeutral %u\n", group_hasNeutral(group));
  printf("  (*) hasInvs    %u\n\n", group_hasInvs(group));
  printf("  (*) neutral Element %u\n", group_neutral(group));
  printf("  (*) isCyclic        %u\n", group_isCyclic(group));
  printf("  (*) isCommutative   %u\n\n", group_isCommutative(group));
  Map_uint16 *orderMap = group_orderDist_alloc(group);
  printf("  (*) order distribution:\n");
  mapui16_printToWidth(orderMap, 80, 2);
  mapui16_free(orderMap);
}

#include <stdlib.h>
#include <stdio.h>

#include "group.h"


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
    if(group_elementOrder(group, *at_uint16(group->set, i)) == n) return 1;
  }
  return 0;
}

uint16_t group_elementOrder(Group *group, uint16_t ele) {
  uint16_t r = ele;
  uint16_t ord = 1;
  while(r != 0) {
    r = gop(group, r, ele);
    ord++;
#ifdef BOUNDS_CHECK
    if(ord > group_order(group)) {
      printError("error: element order not finite!");
      exit(1);
    }
#endif
  }
  return ord;
}

uint16_t group_elementOrderi(Group *group, uint16_t ind) {
  uint16_t r = ind;
  uint16_t ord = 1;
  while(r != 0) {
    r = gopi(group, r, ind);
    ord++;
#ifdef BOUNDS_CHECK
    if(ord > group_order(group)) {
      printError("error: element order not finite!");
      exit(1);
    }
#endif
  }
  return ord;
}

void group_setInvs(Group *group) {
  uint16_t i, j;
  uint16_t n = group_order(group);
  for(i = 0; i < n; i++) {
    for(j = 0; j < n; j++) {
      if(gopi(group, i, j) == 0) {
        *at_uint16(group->invs, i) = *at_uint16(group->set, j);
        break;
      }
    }
  }
}

uint16_t group_neutral(Group *group) {
  return *at_uint16(group->set, group_neutral(group));
}

uint32_t group_neutrali(Group *group) {
  uint32_t i;
  uint32_t neutralInd = 0xffffffff;
  for(i = 0; i < group_order(group); i++) {
    if(gopi(group, i, 0) == 0) {
      neutralInd = i;
      break;
    }
  }
  return neutralInd;
}

Group *group_alloc(uint16_t order, bool indexed) {
  Group *group = malloc(sizeof(Group));
  group->indexed = indexed;
  group->set = allocArray_uint16(order);
  group->gtab = allocArray_uint16(order * order);
  group->invs = allocArray_uint16(order);
  return group;
}

void group_free(Group *group) {
  freeArray_uint16(group->invs);
  freeArray_uint16(group->gtab);
  freeArray_uint16(group->set);
  free(group);
}

bool group_isValid(Group *group) {
  if(!group_hasValidOp(group)) return 0;
  if(!group_isAsoc(group)) return 0;
  if(!group_hasNeutral(group)) return 0;
  if(!group_hasInvs(group)) return 0;
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
  uint32_t i, j;
  uint32_t invInd = 0xffffffff;
  uint16_t neuInd = group_neutrali(group);
  for(i = 0; i < n; i++) {
    for(j = 0; j < n; j++) { // find inv candidate
      if(gopi(group, i, j) == neuInd) {
        invInd = j;
        break;
      }
    }
    if(invInd >= n) { // valid index?
      return 0;
    }
    if(gopi(group, invInd, i) != neuInd) { // check other way
      return 0;
    }
    if(*at_uint16(group->invs, i) != *at_uint16(group->set, invInd)) {
      return 0;
    }
    invInd = 0xffffffff;
  }
  return 1;
}

bool group_hasValidOp(Group *group) {
  uint32_t n = group_order(group);
  Array_uint16 *row = copyArray_uint16(group->set);
  Array_uint16 *col = copyArray_uint16(group->set);
  uint32_t i, j;
  for(i = 0; i < n; i++) {
    for(j = 0; j < n; j++) {
      *at_uint16(row, gopi(group, i, j)) = 0xffff;
      *at_uint16(col, gopi(group, j, i)) = 0xffff;
    }
    for(j = 0; j < n; j++) {
      if(*at_uint16(row, j) != 0xffff || *at_uint16(col, j) != 0xffff) {
        freeArray_uint16(row);
        freeArray_uint16(col);
        return 0;
      }
      *at_uint16(row, j) = 0; // just not 0xffff
      *at_uint16(col, j) = 0; // just not 0xffff
    }
  }
  freeArray_uint16(row);
  freeArray_uint16(col);
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

Map_uint16 *getOrderDistribution(Group *group) {
  uint32_t n = group_order(group);
  Array_uint16 *primeFac = getFactors_alloc(n);
  Array_uint16 *orderCounts = allocArray_uint16(primeFac->size);
  fillArray_uint16(orderCounts, 0);
  uint32_t i, orderi;
  for(i = 0; i < n; i++) {
    orderi = group_elementOrderi(group, i);
    (*at_uint16(orderCounts, indexof_uint16(primeFac, orderi)))++;
  }
  Map_uint16 *map = allocMap_ref_uint16(primeFac->size, 0, primeFac,
                                        orderCounts);
  return map;
}

void group_print(Group *group) {
  uint32_t n = group_order(group);
  char *pstring = malloc(n * n * 6 + n * 6 * 2 + 100); // just a guess
  char *valid;
  if(group_isValid(group)) valid = "valid";
  else valid = "INVALID";
  printf("Group of order %u (%s)\n", n, valid);
  sprintArraySquare_uint16(pstring, group->gtab, n);
  printf("%s", pstring);
  free(pstring);
}

void group_printSummary(Group *group) {
  uint32_t n = group_order(group);
  printf("\nSummary of group candidate of order %u\n", n);
  printf("  (*) hasGroupOp %u\n", group_hasValidOp(group));
  printf("  (*) isAssoc    %u\n", group_isAsoc(group));
  printf("  (*) hasNeutral %u\n", group_hasNeutral(group));
  printf("  (*) hasInvs    %u\n\n", group_hasInvs(group));
  printf("  (*) neutral Element %u\n", group_neutral(group));
  printf("  (*) isCyclic        %u\n", group_isCyclic(group));
  printf("  (*) isCommutative   %u\n\n", group_isCommutative(group));
  Map_uint16 *orderMap = getOrderDistribution(group);
  char pstring[1000];
  printf("  (*) order distribution:\n");
  printArray_uint16(pstring, orderMap->domain);
  printArray_uint16(pstring, orderMap->codomain);
  freeMap_uint16(orderMap);
}

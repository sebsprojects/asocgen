#include <stdlib.h>
#include <stdio.h>

#include "group.h"


bool isCommutative(Group *group) {
  uint32_t i, j;
  uint16_t n = order(group);
  for(i = 0; i < n; i++) {
    for(j = 0; j < n; j++) {
      if(gop(group, i, j) != gop(group, j, i)) return 0;
    }
  }
  return 1;
}

bool isCyclic(Group *group) {
  uint32_t i;
  uint16_t n = order(group);
  for(i = 0; i < n; i++) {
    if(elementOrder(group, i) == n) return 1;
  }
  return 0;
}

uint16_t elementOrder(Group *group, uint16_t ele) {
  uint16_t r = ele;
  uint16_t ord = 1;
  while(r != 0) {
    r = gop(group, r, ele);
    ord++;
    #ifdef BOUNDS_CHECK
    if(ord > order(group)) {
      printError("error: element order not finite!");
      exit(1);
    }
    #endif
  }
  return ord;
}

uint16_t compInv(Group *group, uint16_t ele) {
  uint32_t i;
  uint16_t n = order(group);
  for(i = 0; i < n; i++) {
    if(gop(group, ele, i) == 0) return i;
  }
  return 0xffff;
}

Group *allocGroup(uint16_t order) {
  Group *group = malloc(sizeof(Group));
  group->set = allocArray_uint16(order);
  group->invs = allocArray_uint16(order);
  group->mtab = allocArray_uint16(order * order);
  return group;
}

void freeGroup(Group *group) {
  freeArray_uint16(group->mtab);
  freeArray_uint16(group->invs);
  freeArray_uint16(group->set);
  free(group);
}

bool isValid(Group *group) {
  if(!hasNeutral(group)) return 0;
  if(!hasInvs(group)) return 0;
  if(!isAsoc(group)) return 0;
  return 1;
}

bool hasNeutral(Group *group) {
  uint32_t i;
  for(i = 0; i < order(group); i++) {
    if(*at_uint16(group->mtab, i) != *at_uint16(group->set, i)) return 0;
  }
  return 1;
}

bool hasInvs(Group *group) {
  uint32_t n = order(group);
  uint32_t i;
  uint16_t a, b;
  for(i = 0; i < n; i++) {
    a = *at_uint16(group->set, i);
    b = *at_uint16(group->invs, i);
    if(gop(group, a, b) != 0) return 0;
  }
  return 1;
}

bool isAsoc(Group *group) {
  uint16_t a, b, c, ab, ab_c, bc, a_bc;
  uint32_t n = order(group);
  for(a = 0; a < n; a++) {
    for(b = 0; b < n; b++) {
      for(c = 0; c < n; c++) {
	ab = gop(group, a, b);
	bc = gop(group, b, c);
        ab_c = gop(group, ab, c);
	a_bc = gop(group, a, bc);
	if(ab_c != a_bc) return 0;
      }
    }
  }
  return 1;
}

void printGroup(Group *group) {
  uint32_t n = order(group);
  char *pstring = malloc(n * n * 6 + n * 6 * 2 + 100); // just a guess
  sprintArray_uint16(pstring, group->set);
  char *valid;
  if(isValid(group)) valid = "valid";
  else valid = "INVALID";
  printf("Group of order %u (%s)\n", n, valid);
  sprintArraySquare_uint16(pstring, group->mtab, n);
  printf("%s", pstring);
  free(pstring);
}

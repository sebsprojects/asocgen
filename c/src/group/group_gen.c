#include "group_gen.h"


/*
  input res and util can have abitrary content

  util contains indices in random order, filled from the start
  res  contains elements stored at their index, rest is 0xfffff

  while number of ele in res has changed:
    compute all possible products
    store new element(s) in res, new indice(s) in util

 */
void generateFrom_noalloc(Group *group,
                          Array_uint16 *set,    // set to generate from
                          Array_uint16 *res,    // result
                          Array_uint16 *util)   // incremental fill
{
  uint32_t n = order(group);
  bool commutative = isCommutative(group);
#ifdef BOUNDS_CHECK
  if(n != res->size || n != util->size) {
    printError("error: generateFrom_noalloc size mismatch");
    exit(1);
  }
#endif
  uint32_t i, j;
  uint16_t ind, ele;
  uint32_t filled = 0;
  fillArray_uint16(res, 0xffff); // clear res
  //fillArray_uint16(util, 0xffff); NOT NECESSARY
  for(i = 0; i < set->size; i++) {
    ele = *at_uint16(set, i);
    if(ele == 0xffff) break;    // accept 0xffff as stop marker
    filled++;
    ind = indexof_uint16(group->set, ele);
    *at_uint16(res, ind) = ele; // set res[index(ele)] = ele
    *at_uint16(util, i) = ind;  // set utl[i] = ele
  }
  uint32_t prevFilled = 0;

  uint16_t a, b;
  while(filled != prevFilled) {
    prevFilled = filled;
    for(i = 0; i < filled; i++) {
      for(j = 0; j <= i; j++) {
        a = *at_uint16(util, i);
        b = *at_uint16(util, j);
        ind = gopi(group, a, b);
        ele = *at_uint16(group->set, ind);
        if(*at_uint16(res, ind) == 0xffff) {
          *at_uint16(res, ind) = ele;
          *at_uint16(util, filled) = ind;
          filled++;
        }
        // TODO HELP BRANCH PREDICT AND PUT OUTSIDE LOOP
        if(!commutative) {
          ind = gopi(group, b, a);
          ele = *at_uint16(group->set, ind);
          if(*at_uint16(res, ind) == 0xffff) {
            *at_uint16(res, ind) = ele;
            *at_uint16(util, filled) = ind;
            filled++;
          }
        }
      }
    }
  }
}

Array_uint16 *generateFrom_alloc(Group *group, Array_uint16 *set) {
  Array_uint16 *res = allocArray_uint16(order(group));
  Array_uint16 *util = allocArray_uint16(order(group));
  generateFrom_noalloc(group, set, res, util);
  freeArray_uint16(util);
  return res;
}

void truncGeneratedSet(Group *group, Array_uint16 *res, bool shrink) {
  uint32_t n = order(group);
  uint32_t i, j;
  uint16_t *a, *b;
  uint16_t last = 0;
  for(i = 0; i < n; i++) {
    a = at_uint16(res, i);
    if(*a != 0xffff) { // if element is not 0xffff nothing is to be done
      last = i;
      continue;
    }
    for(j = i + 1; j < n; j++) { // find the next element != to 0xffff
      b = at_uint16(res, j);
      if(*b == 0xffff) {
        continue;
      } else { // found one, set a to b and clear b
        *a = *b;
        *b = 0xffff;
        last = i;
        break;
      }
    }
  }
  if(shrink) shrink_uint16(res, last + 1);
}

/*
  Checks if 0xffff is not contained in the array
 */
inline bool isComplete(Array_uint16 *set) {
  int32_t i;
  for(i = set->size - 1; i >= 0; i--) {
    if(*at_uint16(set, i) == 0xffff) return 0;
  }
  return 1;
}

/*
  Set indices in set according to the permutation supplied
 */
inline void initFromPerm(Group *group, Array_uint16 *perm, Array_uint16 *set,
                         uint32_t pn) {
  uint32_t i;
  for(i = 0; i < pn; i++) {
    *at_uint16(set, i) = *at_uint16(group->set, *at_uint16(perm, i));
  }
}

bool generatingSet(Group *group,
                   Array_uint16 *res,
                   Array_uint16 *perm,
                   Array_uint16 *util1,
                   Array_uint16 *util2,
                   uint32_t pn)
{
  uint32_t n = order(group);
#ifdef BOUNDS_CHECK
  if(res->size != n || perm->size != n ||
     util1->size != n || util2->size != n) {
    printError("error: minGeneratingSet_noalloc size mismatch");
    exit(1);
  }
#endif
  fillArray_uint16(res, 0xffff); // prep set to generate from
  bool compl = 0;
  bool permPossible = 1;
  while(!compl && permPossible) {
    initFromPerm(group, perm, res, pn);
    generateFrom_noalloc(group, res, util1, util2);
    permPossible = shiftPerm(perm, n - 1);
    compl = isComplete(util1);
  }
  if(!compl) {
    fillArray_uint16(res, 0xffff);
  }
  return permPossible;
}

bool minGeneratingSet_noalloc(Group *group,
                              Array_uint16 *res,
                              Array_uint16 *perm,
                              Array_uint16 *util1,
                              Array_uint16 *util2)
{
  uint32_t i;
  uint32_t n = order(group);
  uint32_t pn = 0; // pn-subsets as start, determined in the following
  for(i = 0; i < n; i++) {
    if(*at_uint16(perm, i) != 0xffff) pn++;
  }
  bool permPossible = 1;
  bool compl = 0;
  for(i = pn; i <= n; i++) {
    while(permPossible && !compl) {
      permPossible = generatingSet(group, res, perm, util1, util2, i);
      compl = *at_uint16(res, 0) != 0xffff; // 1-ele is always there at ind=0
    }
    if(!permPossible && i < n) { // go to pn + 1
      initPerm(perm, i + 1); // initialize the pn + 1 perm
      permPossible = 1;
    }
    if(compl) return permPossible;
  }
  if(!compl) fillArray_uint16(res, 0xffff);
  return permPossible;
}

Array_uint16 *minGeneratingSet_alloc(Group *group) {
  uint32_t n = order(group);
  Array_uint16 *res = allocArray_uint16(n);
  Array_uint16 *perm = allocArray_uint16(n);
  Array_uint16 *util1 = allocArray_uint16(n);
  Array_uint16 *util2 = allocArray_uint16(n);
  initPerm(perm, 1);
  minGeneratingSet_noalloc(group, res, perm, util1, util2);
  freeArray_uint16(perm);
  freeArray_uint16(util1);
  freeArray_uint16(util2);
  return res;
 }

/*
  Fills, starting with 0: perm[i] = i, ending (including) ind - 1
 */
inline void toIdBefore(Array_uint16 *perm, uint16_t ind) {
  uint32_t i;
  for(i = 0; i < ind; i++) {
    *at_uint16(perm, i) = i;
  }
}


/*
  since the perm can be 0xffff terminated, more complicated cases need to be
  considered (ie. is t0 end element / is t1 end element)
*/
bool shiftPerm(Array_uint16 *perm, uint16_t max) {
  uint32_t i;
  uint16_t *ele_t0;
  uint16_t *ele_t1;
  for(i = 0; i < perm->size - 1; i++) {
    ele_t0 = at_uint16(perm, i);
    ele_t1 = at_uint16(perm, i + 1);

    if(*ele_t1 == 0xffff) { // t0 is end element, inc t0 if possible
      if(*ele_t0 < max) {
        (*ele_t0)++;
        toIdBefore(perm, i);
        return 1;
      } else {
        return 0;
      }
    } else if (*ele_t1 - *ele_t0 > 1) { // test if case 1 is applicable
      (*ele_t0)++;
      toIdBefore(perm, i);
      return 1;
    } else if (i == perm->size - 2) { // t1 is end element, inc t1 if possible
      if(*ele_t1 < max) {
        toIdBefore(perm, i + 1);
        (*ele_t1)++;
        return 1;
      } else {
        return 0;
      }
    }
  }
  return 0;
}

void initPerm(Array_uint16 *perm, uint16_t n) {
#ifdef BOUNDS_CHECK
  if(n > perm->size) {
    printError("error: initPerm, n too large for array\n");
    exit(1);
  }
#endif
  fillArray_uint16(perm, 0xffff);
  toIdBefore(perm, n);
}

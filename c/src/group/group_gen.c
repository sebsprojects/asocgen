#include "group_gen.h"


/*
  input res and util can have abitrary content

  util contains indices in random order, filled from the start
  res  contains elements stored at their index, rest is 0xfffff

  while number of ele in res has changed:
    compute all possible products
    store new element(s) in res, new indice(s) in util

 */
void group_generateFrom_noalloc(Group *group,
                                Array_uint16 *set,    // set to generate from
                                Array_uint16 *res,    // result
                                Array_uint16 *util)   // incremental fill
{
  bool commutative = group_isCommutative(group);
#ifdef BOUNDS_CHECK
  uint32_t n = group_order(group);
  if(n != res->size || n != util->size) {
    printError("error: generateFrom_noalloc size mismatch");
    exit(1);
  }
#endif
  uint32_t i, j;
  uint16_t ind, ele;
  uint32_t filled = 0;
  aui16_fill(res, 0xffff); // clear res
  //aui16_fill(util, 0xffff); NOT NECESSARY
  for(i = 0; i < set->size; i++) {
    ele = *aui16_at(set, i);
    if(ele == 0xffff) break;    // accept 0xffff as stop marker
    filled++;
    ind = aui16_indexOf(group->set, ele);
    *aui16_at(res, ind) = ele; // set res[index(ele)] = ele
    *aui16_at(util, i) = ind;  // set utl[i] = ele
  }
  uint32_t prevFilled = 0;

  uint16_t a, b;
  while(filled != prevFilled) {
    prevFilled = filled;
    for(i = 0; i < filled; i++) {
      for(j = 0; j <= i; j++) {
        a = *aui16_at(util, i);
        b = *aui16_at(util, j);
        ind = gopi(group, a, b);
        ele = *aui16_at(group->set, ind);
        if(*aui16_at(res, ind) == 0xffff) {
          *aui16_at(res, ind) = ele;
          *aui16_at(util, filled) = ind;
          filled++;
        }
        // TODO HELP BRANCH PREDICT AND PUT OUTSIDE LOOP
        if(!commutative) {
          ind = gopi(group, b, a);
          ele = *aui16_at(group->set, ind);
          if(*aui16_at(res, ind) == 0xffff) {
            *aui16_at(res, ind) = ele;
            *aui16_at(util, filled) = ind;
            filled++;
          }
        }
      }
    }
  }
}

Array_uint16 *group_generateFrom_alloc(Group *group, Array_uint16 *set) {
  Array_uint16 *res = aui16_alloc(group_order(group));
  Array_uint16 *util = aui16_alloc(group_order(group));
  group_generateFrom_noalloc(group, set, res, util);
  aui16_free(util);
  return res;
}

void group_truncGeneratedSet(Array_uint16 *res, bool shrink) {
  uint32_t i, j;
  uint16_t *a, *b;
  uint16_t last = 0;
  for(i = 0; i < res->size; i++) {
    a = aui16_at(res, i);
    if(*a != 0xffff) { // if element is not 0xffff nothing is to be done
      last = i;
      continue;
    }
    for(j = i + 1; j < res->size; j++) { // find the next element != to 0xffff
      b = aui16_at(res, j);
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
  if(shrink) {
    aui16_shrink(res, last + 1);
  }
}

/*
  Checks if 0xffff is not contained in the array
 */
inline bool isComplete(Array_uint16 *set) {
  int32_t i;
  for(i = set->size - 1; i >= 0; i--) {
    if(*aui16_at(set, i) == 0xffff) return 0;
  }
  return 1;
}

/*
  Set indices in set according to the permutation supplied
 */
inline void initFromBinom(Group *group, Array_uint16 *binom,Array_uint16 *set,
                         uint32_t pn) {
  uint32_t i;
  for(i = 0; i < pn; i++) {
    *aui16_at(set, i) = *aui16_at(group->set, *aui16_at(binom, i));
  }
}

bool group_generatingSet(Group *group,
                         Array_uint16 *res,
                         Array_uint16 *binom,
                         Array_uint16 *util1,
                         Array_uint16 *util2,
                         uint32_t pn)
{
  uint32_t n = group_order(group);
#ifdef BOUNDS_CHECK
  if(res->size != n || binom->size != n ||
     util1->size != n || util2->size != n) {
    printError("error: minGeneratingSet_noalloc size mismatch");
    exit(1);
  }
#endif
  aui16_fill(res, 0xffff); // prep set to generate from
  bool compl = 0;
  bool binomPossible = 1;
  while(!compl && binomPossible) {
    initFromBinom(group, binom, res, pn);
    group_generateFrom_noalloc(group, res, util1, util2);
    binomPossible = shiftBinom(binom, n - 1);
    compl = isComplete(util1);
  }
  if(!compl) {
    aui16_fill(res, 0xffff);
  }
  return binomPossible;
}

bool group_minGeneratingSet_noalloc(Group *group,
                                    Array_uint16 *res,
                                    Array_uint16 *binom,
                                    Array_uint16 *util1,
                                    Array_uint16 *util2)
{
  uint32_t i;
  uint32_t n = group_order(group);
  uint32_t pn = 0; // pn-subsets as start, determined in the following
  for(i = 0; i < n; i++) {
    if(*aui16_at(binom, i) != 0xffff) pn++;
  }
  bool binomPossible = 1;
  bool compl = 0;
  for(i = pn; i <= n; i++) {
    while(binomPossible && !compl) {
      binomPossible = group_generatingSet(group, res, binom, util1, util2, i);
      compl = *aui16_at(res, 0) != 0xffff; // 1-ele is always there at ind=0
    }
    if(!binomPossible && i < n) { // go to pn + 1
      initBinom(binom, i + 1); // initialize the pn + 1 binom
      binomPossible = 1;
    }
    if(compl) return binomPossible;
  }
  if(!compl) aui16_fill(res, 0xffff);
  return binomPossible;
}

Array_uint16 *group_minGeneratingSet_alloc(Group *group) {
  uint32_t n = group_order(group);
  Array_uint16 *res = aui16_alloc(n);
  Array_uint16 *binom = aui16_alloc(n);
  Array_uint16 *util1 = aui16_alloc(n);
  Array_uint16 *util2 = aui16_alloc(n);
  initBinom(binom, 1);
  group_minGeneratingSet_noalloc(group, res, binom, util1, util2);
  aui16_free(binom);
  aui16_free(util1);
  aui16_free(util2);
  return res;
}

Group *group_generateSubgroup_alloc(Group *group, Array_uint16 *set) {
  Array_uint16 *res = group_generateFrom_alloc(group, set);
  group_truncGeneratedSet(res, 1);
  uint32_t m = res->size;    // Subgroup order
  Group *subgroup = group_alloc(m, 0);
  uint32_t i, j;
  uint16_t a, b;
  for(i = 0; i < m; i++) {
    a = *aui16_at(res, i);
    *aui16_at(subgroup->set, i) = a;
  }
  for(i = 0; i < m; i++) {
    a = *aui16_at(res, i);
    for(j = 0; j <= i; j++) {
      b = *aui16_at(res, j);
      *aui16_at(subgroup->gtab, get2DIndex(m, i, j)) = gop(group, a, b);
      *aui16_at(subgroup->gtab, get2DIndex(m, j, i)) = gop(group, b, a);
    }
  }
  group_setInvs(subgroup);
  aui16_free(res);
  return subgroup;
}

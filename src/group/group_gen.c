#include "group_gen.h"

#include <elfc_perm.h>

#include <stdlib.h>
#include <stdio.h>


// --------------------------------------------------------------------------
// Generating from group elements
// --------------------------------------------------------------------------

/*
 * input res and util can have abitrary content
 *
 * util contains indices in random order, filled from the start
 * res  contains elements stored at their index, rest is 0xfffff
 *
 * while number of ele in res has changed:
 *  compute all possible products
 * store new element(s) in res, new indice(s) in util
 *
 */
void group_generateFrom_noalloc(Group *group,
                                Vecu16 *set,    // set to generate from
                                Vecu16 *res,    // result
                                Vecu16 *util)   // incremental fill
{
  bool isCommu = group_isCommutative(group);
  vecu16_fill(res, 0xffff);
  // fill util with the inds of elements in set, copy elements from set to res
  u16 ele;
  u32 ind = -1;
  i32 num = 0;
  for(i32 i = 0; i < set->size; i++) {
    ele = *vecu16_at(set, i);
    if(ele == 0xffff) {
      break;
    }
    num++;
    vecu16_indexOf(group->set, ele, &ind, 0);
    *vecu16_at(res, ind) = ele;
    *vecu16_at(util, i) = ind;
  }
  i32 prevNum = 0;
  u16 a, b, c;
  // Calculate the product of all elements in util with each other and check
  // if we got any new. If yes, repeat, if not we are done
  while(prevNum != num) {
    prevNum = num;
    for(i32 i = 0; i < num; i++) {
      a = *vecu16_at(util, i);
      i32 startJ = isCommu ? i : 0;
      for(i32 j = startJ; j < num; j++) {
        b = *vecu16_at(util, j);
        c = group_opi(group, a, b);
        if(*vecu16_at(res, c) == 0xffff) { // is this prod new in res?
          ele = *vecu16_at(group->set, c);
          *vecu16_at(res, c) = ele;
          *vecu16_at(util, num) = c;
          num++;
        }
      }
    }
  }
}

bool group_generateFromConstr_noalloc(Group *group,
                                      Vecu16 *set,
                                      Vecu16 *res,
                                      Vecu16 *util,
                                      Vecu16 *orderVec,
                                      u32 subgroupOrder)
{
  bool isCommu = group_isCommutative(group);
  vecu16_fill(res, 0xffff);
  // fill util with the inds of elements in set, copy elements from set to res
  u16 ele;
  u32 ind = -1;
  i32 num = 0;
  for(i32 i = 0; i < set->size; i++) {
    ele = *vecu16_at(set, i);
    if(ele == 0xffff) {
      break;
    }
    num++;
    vecu16_indexOf(group->set, ele, &ind, 0);
    *vecu16_at(res, ind) = ele;
    *vecu16_at(util, i) = ind;
  }
  i32 prevNum = 0;
  u16 a, b, c;
  // Calculate the product of all elements in util with each other and check
  // if we got any new. If yes, repeat, if not we are done
  while(prevNum != num) {
    prevNum = num;
    for(i32 i = 0; i < num; i++) {
      a = *vecu16_at(util, i);
      i32 startJ = isCommu ? i : 0;
      for(i32 j = startJ; j < num; j++) {
        b = *vecu16_at(util, j);
        c = group_opi(group, a, b);
        if(*vecu16_at(res, c) == 0xffff) { // is this prod new in res?
          if(num >= subgroupOrder ||
             subgroupOrder % *vecu16_at(orderVec, c) != 0) {
            vecu16_fill(res, 0xffff);
            return 0;
          }
          ele = *vecu16_at(group->set, c);
          *vecu16_at(res, c) = ele;
          *vecu16_at(util, num) = c;
          num++;
        }
      }
    }
  }
  return 1;
}

Vecu16 *group_generateFrom_alloc(Group *group, Vecu16 *set) {
  u16 n = group_order(group);
  Vecu16* res = vecu16_alloc(n);
  Vecu16* util = vecu16_alloc(n);
  group_generateFrom_noalloc(group, set, res, util);
  vecu16_free(util);
  return res;
}

/*
 * This function works in priciple like group_generateFrom_noalloc but
 * keeps track of how the elements were reached in genDecomp.
 * We do not need a res array as we know that res = group->set since
 * genSet is a generating set of the group
 */
void group_genDecomposition(Group *group,
                            Vecu16 *genSet,     // gen set of group
                            Vecptr *genDecomp)  // result
{
  // fill all vecu16s in genDecomp with 0xffff
  Vecu16 *decomp;
  for(i32 i = 0; i < genDecomp->size; i++) {
    decomp = *vecptr_at(genDecomp, i);
    vecu16_fill(decomp, 0xffff);
  }
  u32 n = group_order(group);
  u32 genSetSize = genSet->size;
  vecu16_indexOf(genSet, 0xffff, &genSetSize, 0);
  bool isCommu = group_isCommutative(group);
  Vecu16 *util = vecu16_alloc(n);
  vecu16_fill(util, 0xffff);
  u16 ele;
  i32 num = 0; // the number of "reached" elements
  // init util and genDecomp with the elements of genSet
  for(i32 i = 0; i < genSetSize; i++) {
    ele = *vecu16_at(genSet, i);
    if(ele == 0xffff) {
      break;
    }
    num++;
    u32 ind = -1;
    vecu16_indexOf(group->set, ele, &ind, 0);
    *vecu16_at(util, i) = ind;
    // Set the decompositions of the generating elements (trivial)
    decomp = *vecptr_at(genDecomp, ind);
    *vecu16_at(decomp, 0) = ele;
  }
  i32 prevNum = 0;
  u16 a, b, c; // all indices
  while(prevNum != num) {
    prevNum = num;
    // generating from all elements in util
    for(i32 i = 0; i < num; i++) {
      a = *vecu16_at(util, i);
      i32 startJ = isCommu ? i : 0;
      for(i32 j = startJ; j < num; j++) {
        b = *vecu16_at(util, j);
        c = group_opi(group, a, b); // compute a * b
        if(!vecu16_contains(util, c, 0)) { // is this prod new in util?
          *vecu16_at(util, num) = c;
          // get the (already known decomps of a and b)
          Vecu16 *decompA = *vecptr_at(genDecomp, a);
          Vecu16 *decompB = *vecptr_at(genDecomp, b);
          // set the decomp of c to the decomp of a concat decomp of b
          decomp = *vecptr_at(genDecomp, c);
          u32 dind = decompA->size;
          vecu16_indexOf(decompA, 0xffff, &dind, 0);
          vecu16_copyInto(decompA, decomp, 0);
          // manual copyInto because we only need a slice until 0xffff
          for(i32 k = 0; k < decompB->size; k++) {
            u16 z = *vecu16_at(decompB, k);
            if(z == 0xffff) {
              break;
            }
            *vecu16_at(decomp, k + dind) = z;
          }
          num++;
        }
      }
    }
  }
  vecu16_free(util);
}


// --------------------------------------------------------------------------
// Generating sets of groups
// --------------------------------------------------------------------------

inline void initGenSetFromBinom(Group *group,
                                Vecu16 *binom,
                                Vecu16 *res,
                                u32 pn)
{
  for(i32 i = 0; i < pn; i++) {
    *vecu16_at(res, i) = *vecu16_at(group->set, *vecu16_at(binom, i));
  }
}

// This just does !vecu16_contains(set, 0xffff) but in reverse order since
// we know the first 0xffff will always be at the end of all other entries
inline bool isCompleteGen(Vecu16 *set)
{
  for(i32 i = set->size - 1; i >= 0; i--) {
    if(*vecu16_at(set, i) == 0xffff) {
      return 0;
    }
  }
  return 1;
}

bool group_generatingSet(Group *group,
                         Vecu16 *res,
                         Vecu16 *binom,
                         Vecu16 *util1,
                         Vecu16 *util2,
                         u32 pn)
{
  u32 n = group_order(group);
  vecu16_fill(res, 0xffff);
  bool genComplete = 0;
  bool binomPossible = 1;
  while(!genComplete && binomPossible) {
    initGenSetFromBinom(group, binom, res, pn);
    // here util1 = res of the generation
    group_generateFrom_noalloc(group, res, util1, util2);
    binomPossible = binom_shift(binom, n - 1, pn, 0);
    // this checks if all elements in group->set were produced by the
    // generating set. If so, we found a generating set for the whole group
    // and not a proper subgroup
    genComplete = isCompleteGen(util1);
  }
  if(!genComplete) {
    vecu16_fill(res, 0xffff);
  }
  return binomPossible;
}

// Careful: the group_generatingSet call increments the binom
bool group_minGeneratingSet_noalloc(Group *group,
                                    Vecu16 *res,
                                    Vecu16 *binom,
                                    Vecu16 *util1,
                                    Vecu16 *util2)
{
  u32 n = group_order(group);
  u32 pn = n;
  vecu16_indexOf(binom, 0xffff, &pn, 0);
  bool binomPossible = 1;
  bool genComplete = 0;
  for(i32 i = pn; i <= n; i++) {
    while(!genComplete && binomPossible) {
      binomPossible = group_generatingSet(group, res, binom, util1, util2, i);
      // generatingSet ensures that if we have a complete gen, then at index 0
      // contains the first element and not 0xffff
      genComplete = *vecu16_at(res, 0) != 0xffff;
    }
    if(!binomPossible && i < n) {
      binom_init(binom, 0, i + 1, 0);
      binomPossible = 1;
    }
    if(genComplete) {
      return binomPossible;
    }
  }
  if(!genComplete) {
    vecu16_fill(res, 0xffff);
  }
  return binomPossible;
}

GenConstrUtils *group_allocSetupConstrUtils(Group* group, u32 orderConstrSize)
{
  u32 n = group_order(group);
  GenConstrUtils *constr = malloc(sizeof(GenConstrUtils));
  constr->groupEleOrders = vecu16_alloc(n);
  constr->orderUtil = vecu16_alloc(orderConstrSize);
  constr->genUtil1 = vecu16_alloc(n);
  constr->genUtil2 = vecu16_alloc(n);
  for(i32 i = 0; i < n; i++) {
    *vecu16_at(constr->groupEleOrders, i) = group_elementOrderi(group, i);
  }
  return constr;
}

void group_freeConstrUtils(GenConstrUtils *constrUtils)
{
  vecu16_free(constrUtils->genUtil2);
  vecu16_free(constrUtils->genUtil1);
  vecu16_free(constrUtils->orderUtil);
  vecu16_free(constrUtils->groupEleOrders);
  free(constrUtils);
}

bool group_minGeneratingSetConstr(Group *group,
                                  Vecu16 *res,
                                  Vecu16 *binom,
                                  Vecu16 *orderConstr,
                                  GenConstrUtils *utils)
{
  u32 n = group_order(group);
  u32 pn = n;
  vecu16_indexOf(binom, 0xffff, &pn, 0); // set pn
  bool binomPossible = 1;
  bool genComplete = 0;
  Vecu16 *eleOrd = utils->groupEleOrders;
  Vecu16 *ordCheck = utils->orderUtil;
  // iterate over all possible pns starting from pn
  while(binomPossible) {
    // Order constraint check if supplied. Write the order of all
    // binom-elements to ordCheck, sort it and compare for vector equality
    for(i32 j = 0; j < pn; j++) {
      *vecu16_at(ordCheck, j) = *vecu16_at(eleOrd, *vecu16_at(binom, j));
    }
    vecu16_sort(ordCheck, 0, ordCheck->size);
    if(vecu16_areEqualVectors(orderConstr, ordCheck)) {
      initGenSetFromBinom(group, binom, res, pn);
      group_generateFrom_noalloc(group, res, utils->genUtil1, utils->genUtil2);
      binomPossible = binom_shift(binom, n - 1, pn, 0);
      genComplete = isCompleteGen(utils->genUtil1);
    } else {
      binomPossible = binom_shift(binom, n - 1, pn, 0);
    }
    if(genComplete) {
      return binomPossible;
    } else {
      vecu16_fill(res, 0xffff);
    }
  }
  return binomPossible;
}

Vecu16 *group_minGeneratingSet_alloc(Group *group)
{
  u32 n = group_order(group);
  Vecu16 *res = vecu16_alloc(n);
  Vecu16 *binom = vecu16_alloc(n);
  Vecu16 *util1 = vecu16_alloc(n);
  Vecu16 *util2 = vecu16_alloc(n);
  binom_init(binom, 0, 1, 0);
  group_minGeneratingSet_noalloc(group, res, binom, util1, util2);
  vecu16_free(util2);
  vecu16_free(util1);
  vecu16_free(binom);
  return res;
}


// --------------------------------------------------------------------------
// Generating a subgroup from elements
// --------------------------------------------------------------------------

Group *group_expandSubgroupFromSet_alloc(Group *group, Vecu16 *res)
{
  u32 m = res->size;
  Group *subgroup = group_alloc(m, 0);
  vecu16_copyInto(res, subgroup->set, 0);
  u16 a, b, ab;
  bool isCommu = group_isCommutative(group);
  for(i32 i = 0; i < m; i++) {
    a = *vecu16_at(res, i);
    for(i32 j = i; j < m; j++) {
      b = *vecu16_at(res, j);
      ab = group_op(group, a, b);
      *vecu16_at(subgroup->gtab, get2DIndex(m, i, j)) = ab;
      if(isCommu) {
        *vecu16_at(subgroup->gtab, get2DIndex(m, j, i)) = ab;
      } else {
        *vecu16_at(subgroup->gtab, get2DIndex(m, j, i)) = group_op(group, b, a);
      }
    }
  }
  return subgroup;
}


Group *group_generateSubgroup_alloc(Group *group, Vecu16 *set)
{
  Vecu16 *res = group_generateFrom_alloc(group, set);
  group_truncGeneratedSet(res, 1);
  Group *subgroup = group_expandSubgroupFromSet_alloc(group, res);
  vecu16_free(res);
  return subgroup;
}


// --------------------------------------------------------------------------
// Utility
// --------------------------------------------------------------------------

void group_truncGeneratedSet(Vecu16 *res, bool shrink) {
  u16 *a, *b;
  u16 last = 0;
  for(i32 i = 0; i < res->size; i++) {
    a = vecu16_at(res, i);
    if(*a != 0xffff) { // if element is not 0xffff nothing is to be done
      last = i;
      continue;
    }
    for(i32 j = i + 1; j < res->size; j++) { // find next element != to 0xffff
      b = vecu16_at(res, j);
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
    vecu16_resize(res, last + 1);
  }
}

void group_printMinGenSetOrderDist(Group *group)
{
  u32 n = group_order(group);
  Vecu16 *res = vecu16_alloc(n);
  Vecu16 *binom = vecu16_alloc(n);
  Vecu16 *util1 = vecu16_alloc(n);
  Vecu16 *util2 = vecu16_alloc(n);
  binom_init(binom, 0, 1, 0);
  bool ok = 1;
  u32 minSize = 0;
  u32 ind = 0;
  while(ok) {
    // check if we can continue shifting biom
    ok = group_minGeneratingSet_noalloc(group, res, binom, util1, util2);
    if(*vecu16_at(res, 0) != 0xffff) {
      if(minSize == 0) {
        vecu16_indexOf(res, 0xffff, &minSize, 0);
      }
      // exit after visiting all min gen sets
      vecu16_indexOf(res, 0xffff, &ind, 0);
      if(ind > minSize) {
        break;
      }
      printf("Found min gen set of size with orders:");
      for(i32 i = 0; i < minSize; i++) {
        u16 ord = group_elementOrder(group, *vecu16_at(res, i));
        printf(" %i", ord);
      }
      printf("\n");
    }
  }
  vecu16_free(util2);
  vecu16_free(util1);
  vecu16_free(binom);
  vecu16_free(res);
}

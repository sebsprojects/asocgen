#include "group_hom.h"

#include <elfc_vecptr.h>

#include <stdlib.h>

#include "group_gen.h"


// ---------------------------------------------------------------------------
// Management
// ---------------------------------------------------------------------------

GroupHom *group_allocHom_ref(Group *from, Group *to, Mapu16 *map)
{
  GroupHom *hom = malloc(sizeof(GroupHom));
  hom->from = from;
  hom->to = to;
  hom->map = map;
  return hom;
}

void group_freeHom_ref(GroupHom *hom)
{
  free(hom);
}


// ---------------------------------------------------------------------------
// Information
// ---------------------------------------------------------------------------

bool group_isValidHom(GroupHom *hom)
{
  if(!vecu16_areEqualSets(hom->map->domain, hom->from->set)) {
    return 0;
  }
  if(!vecu16_areEqualSets(hom->map->codomain, hom->to->set)) {
    return 0;
  }
  return group_hasHomProp(hom);
}

// TODO: We do not need to check i, j and j, i if hom->from is commutative
bool group_hasHomProp(GroupHom *hom)
{
  u32 n = group_order(hom->from);
  u16 a, b, x, y;
  for(i32 i = 0; i < n; i++) {
    a = *vecu16_at(hom->map->domain, i);
    for(i32 j = 0; j < n; j++) {
      b = *vecu16_at(hom->map->domain, j);
      x = mapu16_mapEle(hom->map, group_op(hom->from, a, b));
      y = group_op(hom->to,
                   mapu16_mapInd(hom->map, i),
                   mapu16_mapInd(hom->map, j));
      if(x != y) {
        return 0;
      }
    }
  }
  return 1;
}

bool group_isIsomorphism(GroupHom *hom)
{
  return group_order(hom->from) == group_order(hom->to) &&
         mapu16_isInjective(hom->map) &&
         group_isValidHom(hom);
}


// ---------------------------------------------------------------------------
// Working with partial homomorphisms defined only on generating sets
// ---------------------------------------------------------------------------

// TODO: See above
bool group_hasHomPropFromGen(GroupHom *hom, Vecu16 *genFrom)
{
  u32 m = genFrom->size;
  u16 a, b, x, y;
  for(i32 i = 0; i < m; i++) {
    a = *vecu16_at(genFrom, i);
    for(i32 j = 0; j < m; j++) {
      b = *vecu16_at(genFrom, j);
      x = mapu16_mapEle(hom->map, group_op(hom->from, a, b));
      y = group_op(hom->to,
                   mapu16_mapInd(hom->map, i),
                   mapu16_mapInd(hom->map, j));
      if(x != y) {
        return 0;
      }
    }
  }
  return 1;
}

/*
 * We have h(g) = y for g in a generating set and need to find
 * h(x) for any x in hom->from
 */
void group_completeHomFromGen(GroupHom *hom)
{
  u32 n = group_order(hom->from);
  Vecu16 *genSet = vecu16_alloc(n);
  u16 ele;
  u32 num = 0;
  // Find the genSet from hom->map, i.e. all elements != 0xffff in the domain
  for(i32 i = 0; i < hom->map->domain->size; i++) {
    ele = *vecu16_at(hom->map->domain, i);
    if(ele != 0xffff) {
      *vecu16_at(genSet, num) = ele;
      num++;
    }
  }
  vecu16_resize(genSet, num);
  u16 x, g, mappedProd;
  u32 mapInd = 0;
  // alloc a vector of Vecu16* to store the genDecomposition for each group ele
  Vecptr *genDecompVec = vecptr_alloc(n);
  for(i32 i = 0; i < n; i++) {
    *vecptr_at(genDecompVec, i) = vecu16_alloc(genSet->size * n);
  }
  // calculate the decomp
  group_genDecomposition(hom->from, genSet, genDecompVec);
  Vecu16 *genDecomp;
  // For each group element and its decomp, calculate the image under the
  // homomorphism by evaluating the decomp sequence and multiplying it together
  for(i32 i = 0; i < n; i++) {
    // group element at index i has decomp at index i
    x = *vecu16_at(hom->from->set, i);
    if(vecu16_contains(genSet, x, 0)) { // skip ele of genSet, already in map
      continue;
    }
    genDecomp = *vecptr_at(genDecompVec, i);
    mappedProd = *vecu16_at(genDecomp, 0);
    for(i32 j = 1; j < genDecomp->size; j++) {
      g = *vecu16_at(genDecomp, j);
      if(g == 0xffff) { // end of decomp is reached
        break;
      }
      mappedProd = group_op(hom->to, mappedProd,
                            mapu16_mapEle(hom->map, g));
    }
    // find the next free index in map->domain. This is neccessary since it
    // might contain the elements of genSet at abitrary indices
    while(*vecu16_at(hom->map->domain, mapInd) != 0xffff) {
      mapInd++;
    }
    *vecu16_at(hom->map->domain, mapInd) = x;
    *vecu16_at(hom->map->codomain, mapInd) = mappedProd;
  }
  for(i32 i = 0; i < n; i++) {
    vecu16_free(*vecptr_at(genDecompVec, i));
  }
  vecptr_free(genDecompVec);
  vecu16_free(genSet);
}

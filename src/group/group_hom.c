#include "group_hom.h"

#include <elfc_perm.h>

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
// Gen
// ---------------------------------------------------------------------------

/*
 * We have h(g) = y for g in a generating set and need to find
 * h(x) for any x in hom->from
 */
// TODO: Depends on genSet being correctly sized and does not allow for
// 0xffff termination
void group_completeMapFromGen(GroupHom *hom,
                              Vecu16 *genSet,
                              Vecptr *genDecompVec)
{
  u32 n = group_order(hom->from);
  u16 g = 0xffff;
  u16 mappedProd = 0xffff;
  Vecu16 *genDecomp;
  // For each group element and its decomp, calculate the image under the
  // homomorphism by evaluating the decomp sequence and multiplying it together
  for(i32 i = 0; i < n; i++) {
    // skip generating element already set in the beginning of the map
    if(vecu16_contains(genSet, *vecu16_at(hom->from->set, i), 0)) {
      continue;
    }
    // group element at index i has decomp at index i
    genDecomp = *vecptr_at(genDecompVec, i);
    g = *vecu16_at(genDecomp, 0);
    mappedProd = mapu16_mapEle(hom->map, g);
    for(i32 j = 1; j < genDecomp->size; j++) {
      g = *vecu16_at(genDecomp, j);
      if(g == 0xffff) { // end of decomp is reached
        break;
      }
      mappedProd = group_op(hom->to, mappedProd,
                            mapu16_mapEle(hom->map, g));
    }
    *vecu16_at(hom->map->codomain, i) = mappedProd;
  }
}

HomIsoUtils *group_allocSetupIsoUtils(GroupHom *hom, Vecu16 *genFrom)
{
  u32 n = group_order(hom->from);
  u32 m = genFrom->size; // TODO: Fails badly if genFrom is 0xffff term
  HomIsoUtils *utils = malloc(sizeof(HomIsoUtils));
  utils->genTo = vecu16_alloc(m);
  utils->mperm = vecu16_alloc(m);
  utils->genFromOrders = vecu16_alloc(m);
  utils->genDecompVec = vecptr_alloc(n);
  utils->genOrderConstr = vecu16_alloc(m);
  utils->genConstrUtils = group_allocSetupConstrUtils(hom->from, m);
  for(i32 i = 0; i < n; i++) {
    *vecptr_at(utils->genDecompVec, i) = vecu16_alloc(n);
  }
  group_genDecomposition(hom->from, genFrom, utils->genDecompVec);
  u16 ord = 0;
  for(i32 i = 0; i < m; i++) {
    ord = group_elementOrder(hom->from, *vecu16_at(genFrom, i));
    *vecu16_at(utils->genFromOrders, i) = ord;
    *vecu16_at(utils->genOrderConstr, i) = ord;
  }
  vecu16_sort(utils->genOrderConstr, 0, utils->genOrderConstr->size);
  return utils;
}

void group_freeIsoUtils(HomIsoUtils *isoUtils)
{
  group_freeConstrUtils(isoUtils->genConstrUtils);
  for(i32 i = 0; i < isoUtils->genDecompVec->size; i++) {
    vecu16_free(*vecptr_at(isoUtils->genDecompVec, i));
  }
  vecptr_free(isoUtils->genDecompVec);
  vecu16_free(isoUtils->genOrderConstr);
  vecu16_free(isoUtils->genFromOrders);
  vecu16_free(isoUtils->genTo);
  vecu16_free(isoUtils->mperm);
  free(isoUtils);
}

bool group_checkForIsomorphismFromGen(GroupHom *hom,
                                      Vecu16 *genSet,
                                      Vecu16 *binom,
                                      HomIsoUtils *isoUtils)
{
  bool binomOk = group_minGeneratingSetConstr(hom->to,
                                              isoUtils->genTo,
                                              binom,
                                              isoUtils->genOrderConstr,
                                              isoUtils->genConstrUtils);
  // minGenSetConstr may not find a generating set after the next-to-final
  // possible binom
  if(!binomOk && *vecu16_at(isoUtils->genTo, 0) == 0xffff) {
    return binomOk;
  }
  perm_init(isoUtils->mperm);
  bool mpermOk = 1;
  u32 genSetSize = genSet->size;
  vecu16_indexOf(genSet, 0xffff, &genSetSize, 0);
  u32 mapInd = 0;
  u32 genInd = 0;
  u16 toEle = 0xffff;
  u16 toOrder = 0;
  bool validTry = 0;
  while(mpermOk) {
    validTry = 1;
    vecu16_fill(hom->map->codomain, 0xffff); // might be unnecessary
    for(i32 i = 0; i < genSetSize; i++) {
      mapInd = *vecu16_at(isoUtils->mperm, i);
      toEle = *vecu16_at(isoUtils->genTo, mapInd);
      toOrder = group_elementOrder(hom->to, toEle); // could also be precomp
      if(*vecu16_at(isoUtils->genFromOrders, i) != toOrder) {
        validTry = 0;
        break;
      }
      vecu16_indexOf(hom->map->domain, *vecu16_at(genSet, i), &genInd, 0);
      *vecu16_at(hom->map->codomain, genInd) = toEle;
    }
    if(validTry) {
      group_completeMapFromGen(hom, genSet, isoUtils->genDecompVec);
      bool isInj = mapu16_isInjective(hom->map);
      bool hasHomProp = group_hasHomProp(hom);
      printf("%i %i %i -> %i %i\n", *vecu16_at(binom, 0), *vecu16_at(binom, 1),
             *vecu16_at(binom, 2), isInj, hasHomProp);
    }
    mpermOk = perm_shiftDefault(isoUtils->mperm);
  }
  return binomOk;
}

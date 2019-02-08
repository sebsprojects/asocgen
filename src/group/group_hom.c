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
void group_completeMapFromGen(GroupHom *hom,
                              Vecu16 *genSet,
                              Vecptr *genDecompVec)
{
  u32 n = group_order(hom->from);
  u16 x = 0xffff;
  u16 g = 0xffff;
  u16 mappedProd = 0xffff;
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
    mappedProd = mapu16_mapEle(hom->map, *vecu16_at(genDecomp, 0));
    for(i32 j = 1; j < genDecomp->size; j++) {
      g = *vecu16_at(genDecomp, j);
      if(g == 0xffff) { // end of decomp is reached
        break;
      }
      mappedProd = group_op(hom->to, mappedProd,
                            mapu16_mapEle(hom->map, g));
    }
    // indicies in [0, genSet->size) are already filled by genSet elements
    *vecu16_at(hom->map->domain, genSet->size + i) = x;
    *vecu16_at(hom->map->codomain, genSet->size + i) = mappedProd;
  }
}

HomIsoUtils *group_allocSetupIsoUtils(GroupHom *hom,
                                      Vecu16 *genFrom,
                                      Vecu16 *orderConstr)
{
  u32 n = group_order(hom->from);
  u32 m = genFrom->size;
  HomIsoUtils *utils = malloc(sizeof(HomIsoUtils));
  utils->genSetTo = vecu16_alloc(m);
  utils->mperm = vecu16_alloc(m);
  utils->genDecompVec = vecptr_alloc(n);
  utils->orderConstr = vecu16_copy(orderConstr);
  utils->genConstrUtils = group_allocSetupConstrUtils(hom->from,
                                                      orderConstr->size);
  for(i32 i = 0; i < n; i++) {
    *vecptr_at(utils->genDecompVec, i) = vecu16_alloc(n);
  }
  group_genDecomposition(hom->from, genFrom, utils->genDecompVec);
  return utils;
}

void group_freeIsoUtils(HomIsoUtils *isoUtils)
{
  group_freeConstrUtils(isoUtils->genConstrUtils);
  vecu16_free(isoUtils->orderConstr);
  for(i32 i = 0; i < isoUtils->genDecompVec->size; i++) {
    vecu16_free(*vecptr_at(isoUtils->genDecompVec, i));
  }
  vecptr_free(isoUtils->genDecompVec);
  vecu16_free(isoUtils->mperm);
  vecu16_free(isoUtils->genSetTo);
  free(isoUtils);
}

bool group_checkForIsomorphismFromGen(GroupHom *hom,
                                      Vecu16 *genSet,
                                      Vecu16 *binom,
                                      HomIsoUtils *isoUtils)
{
  bool binomOk = group_minGeneratingSetConstr(hom->to,
                                              isoUtils->genSetTo,
                                              binom,
                                              isoUtils->orderConstr,
                                              isoUtils->genConstrUtils);

  perm_init(isoUtils->mperm);
  bool mpermOk = 1;
  u32 ind = genSet->size;
  u32 genSetSize = vecu16_indexOf(genSet, 0xffff, &ind, 0);
  u32 mapInd = 0;
  while(mpermOk) {
    for(i32 i = 0; i < genSetSize; i++) {
      mapInd = *vecu16_at(isoUtils->mperm, i);
      *vecu16_at(hom->map->codomain, i) = *vecu16_at(isoUtils->genSetTo,
                                                     mapInd);
    }
    group_completeMapFromGen(hom, genSet, isoUtils->genDecompVec);
    if(mapu16_isInjective(hom->map) && group_hasHomProp(hom)) {
      return binomOk;
    }
    mpermOk = perm_shiftDefault(isoUtils->mperm);
  }
  return binomOk;
}

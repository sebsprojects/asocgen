#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <elfc_perm.h>
#include <elfc_mapu16.h>

#include "group/group.h"
#include "group/group_common.h"
#include "group/group_gen.h"
#include "group/group_hom.h"


i32 main() {
  u32 n = 4;
  Group *cn = createCn_alloc(n);
  Group *sn = createSn_alloc(n);

  u32 m = group_order(sn);
  Mapu16 *map = mapu16_alloc(m, 1);
  mapu16_setDefault(map);
  for(i32 i = 0; i < m; i++) {
    *vecu16_at(map->codomain, i) = m - i;
  }
  Group *sncopy = group_getRenamedCopy_alloc(sn, map);
  Vecu16 *genSet = group_minGeneratingSet_alloc(sn);
  Vecu16 *genSetC = group_minGeneratingSet_alloc(sncopy);

  Mapu16 *hommap = mapu16_alloc(m, 0);
  GroupHom *hom = group_allocHom_ref(sn, sncopy, hommap);
  group_findHomFromGen(hom, genSet, genSetC);

  mapu16_free(hommap);
  group_freeHom_ref(hom);
  vecu16_free(genSet);
  vecu16_free(genSetC);
  mapu16_free(map);
  group_free(sncopy);
  group_free(sn);
  group_free(cn);
  return 0;
}

void misc1()
{
  /*
  Vecptr *decompVec = vecptr_alloc(m);
  for(i32 i = 0; i < m; i++) {
    *vecptr_at(decompVec, i) = vecu16_alloc(m * 2); // TODO check in hom
  }
  group_genDecomposition(sncopy, genSet, decompVec);
  u32 ind;
  u32 longestDecomp = 0;
  u32 longestDecompInd = 0;
  Vecu16 *decomp;
  for(i32 i = 0; i < m; i++) {
    //vecu16_print(*vecptr_at(decompVec, i));
    printf("At index i=%i where g=%i\n", i, *vecu16_at(sncopy->set, i));
    decomp = *vecptr_at(decompVec, i);
    vecu16_indexOf(decomp, 0xffff, &ind, 0);
    printf("  Length of decomp: %i\n", ind);
    if(longestDecomp < ind) {
      longestDecomp = ind;
      longestDecompInd = i;
    }
    u16 prod = *vecu16_at(decomp, 0);
    for(i32 j = 1; j < ind; j++) {
      prod = group_op(sncopy, prod, *vecu16_at(decomp, j));
    }
    printf("  Product: %i\n\n", prod);
  }
  printf("Longest Decomp: %i\n", longestDecomp);
  vecu16_print(*vecptr_at(decompVec, longestDecompInd));
  for(i32 i = 0; i < m; i++) {
    vecu16_free(*vecptr_at(decompVec, i));
  }
  vecptr_free(decompVec);
  */
}

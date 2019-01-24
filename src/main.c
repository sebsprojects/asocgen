#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <elfc_perm.h>
#include <elfc_mapu16.h>

#include "group/group.h"
#include "group/group_common.h"
#include "group/group_gen.h"


i32 main() {
  u32 n = 1;
  Group *cn = createCn_alloc(n);
  Group *sn = createSn_alloc(n);
  Group *prod = createDirectProduct_alloc(sn, cn);
  group_printSummary(prod);
  Vecu16 *minGenSet = group_minGeneratingSet_alloc(prod);
  vecu16_print(minGenSet);
  vecu16_free(minGenSet);
  group_free(prod);
  group_free(sn);
  group_free(cn);
  return 0;
}

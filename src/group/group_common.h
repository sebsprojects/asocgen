#ifndef GROUP_COMMON
#define GROUP_COMMON

#include "group.h"

#include <elfc_common.h>


// ---------------------------------------------------------------------------
// Common Groups
// ---------------------------------------------------------------------------

Group *group_createCn_alloc(u32 n);
Group *group_createSn_alloc(u32 n);

Group *group_createDirectProduct_alloc(Group *a, Group *b);

#endif

#ifndef GROUP_COMMON
#define GROUP_COMMON

#include "group.h"

#include <elfc_common.h>


// ---------------------------------------------------------------------------
// Common Groups
// ---------------------------------------------------------------------------

Group *createCn_alloc(u32 n);
Group *createSn_alloc(u32 n);

Group *createDirectProduct_alloc(Group *a, Group *b);

#endif

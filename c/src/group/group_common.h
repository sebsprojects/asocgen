#ifndef GROUP_COMMON
#define GROUP_COMMON

#include <math.h>
#include "../common/common_includes.h"
#include "group.h"


Group *createFromGen(uint32_t n, Array_uint8 *mtab);
Group *createCn(uint32_t n);
Group *createSn(uint32_t n);


// For testing only
// uint32_t notfixedImagesToInt(Map_uint16 *perm);

#endif

#ifndef GROUP_COMMON
#define GROUP_COMMON

#include <math.h>
#include "../common/common_includes.h"
#include "group.h"


Group *createFromGen(uint32_t n, Array_uint8 *mtab);
Group *createCn(uint32_t n);
Group *createSn(uint32_t n);

/*
Group *createAn(uint32_t n);
Group *createDn(uint32_t n);
Group *createGLn(uint32_t q, uint32_t n);
Group *createSLn(uint32_t q, uint32_t n);
*/

#endif

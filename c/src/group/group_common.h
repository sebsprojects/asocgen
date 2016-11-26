#ifndef GROUP_COMMON
#define GROUP_COMMON

#include "../common/common.h"
#include "group.h"


Group *createCn(uint32_t n);
Group *createFromGen(uint32_t n, Array_uint8 *mtab);

/*
Group *createSn(uint32_t n);
Group *createAn(uint32_t n);
Group *createDn(uint32_t n);
Group *createGLn(uint32_t q, uint32_t n);
Group *createSLn(uint32_t q, uint32_t n);
Group *createKlein4();
Group *createQuaternion();
*/

#endif

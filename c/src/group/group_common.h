#ifndef GROUP_COMMON
#define GROUP_COMMON

#include <math.h>
#include "../common/common.h"
#include "group.h"

struct Perm8 {
  uint8_t perm[8];
};
typedef struct Perm8 Perm8;

inline void setPerm8(Perm8 *perm, uint8_t pos, uint8_t val) {
#ifdef BOUNDS_CHECK
  if(pos > 7 || val > 7) {
    printError("setPerm8 out of bounds");
    exit(1);
  }
#endif
  perm->perm[pos] = val;
}

inline uint8_t getPerm8(const Perm8 *perm, uint8_t pos) {
#ifdef BOUNDS_CHECK
  if(pos > 7) {
    printError("getPerm8 out of bounds");
    exit(1);
  }
#endif
  return perm->perm[pos];
}

inline Perm8 multPerm8(const Perm8 *p1, const Perm8 *p2) {
  Perm8 res;
  uint32_t i;
  for(i = 0; i < 8; i++) {
    setPerm8(&res, i, getPerm8(p1, getPerm8(p2, i)));
  }
  return res;
}


Group *createCn(uint32_t n);
Group *createFromGen(uint32_t n, Array_uint8 *mtab);
Group *createSn(uint8_t n);

/*
Group *createAn(uint32_t n);
Group *createDn(uint32_t n);
Group *createGLn(uint32_t q, uint32_t n);
Group *createSLn(uint32_t q, uint32_t n);
Group *createKlein4();
Group *createQuaternion();
*/

#endif

#include "sperm.h"

#include "sarray16.h"

/*
  Fills, starting with 0: perm[i] = i, ending (including) ind - 1
 */
inline void toIdBefore(Array_uint16 *array, uint16_t ind) {
  uint32_t i;
  for(i = 0; i < ind; i++) {
    *aui16_at(array, i) = i;
  }
}

void initBinom(Array_uint16 *array, uint16_t n) {
  aui16_fill(array, 0xffff);
  toIdBefore(array, n);
}

void initPerm(Array_uint16 *array) {
  toIdBefore(array, array->size);
}

/*
  since the binom can be 0xffff terminated, more complicated cases need to be
  considered (ie. is t0 end element / is t1 end element)
*/
bool shiftBinom(Array_uint16 *array, uint16_t max) {
  uint32_t i;
  uint16_t *ele_t0;
  uint16_t *ele_t1;
  for(i = 0; i < array->size - 1; i++) {
    ele_t0 = aui16_at(array, i);
    ele_t1 = aui16_at(array, i + 1);

    if(*ele_t1 == 0xffff) { // t0 is end element, inc t0 if possible
      if(*ele_t0 < max) {
        (*ele_t0)++;
        toIdBefore(array, i);
        return 1;
      } else {
        return 0;
      }
    } else if (*ele_t1 - *ele_t0 > 1) { // test if case 1 is applicable
      (*ele_t0)++;
      toIdBefore(array, i);
      return 1;
    } else if (i == array->size - 2) { // t1 is end element, inc t1 if possible
      if(*ele_t1 < max) {
        toIdBefore(array, i + 1);
        (*ele_t1)++;
        return 1;
      } else {
        return 0;
      }
    }
  }
  return 0;
}

uint32_t binom_getK(Array_uint16 *array) {
  uint32_t i;
  for(i = 0; i < array->size; i++) {
    if(*aui16_at(array, i) == 0xffff) {
      return i;
    }
  }
  return 0xffffffff;
}

uint16_t nextBiggestInd(Array_uint16 *array, uint32_t pos) {
  uint16_t curr = *aui16_at(array, pos);
  uint16_t cand = 0xffff;
  uint32_t ind = 0xffff;
  uint32_t i;
  uint16_t ele;
  for(i = pos + 1; i < array->size; i++) {
    ele = *aui16_at(array, i);
    if(ele > curr && ele < cand) {
      cand = ele;
      ind = i;
    }
  }
  return ind;
}

void switchElements(Array_uint16 *array, uint32_t ind1, uint32_t ind2) {
  uint16_t ele = *aui16_at(array, ind1);
  *aui16_at(array, ind1) = *aui16_at(array, ind2);
  *aui16_at(array, ind2) = ele;
}

bool shiftPerm(Array_uint16 *array) {
  int32_t pos = array->size - 2;
  uint16_t nbi;
  while(pos >= 0) {
    nbi = nextBiggestInd(array, pos);
    if(nbi != 0xffff) {
      switchElements(array, pos, nbi);
      aui16_sort(array, pos + 1, array->size - 1);
      return 1;
    } else {
      pos = pos - 1;
    }
  }
  return 0;
}

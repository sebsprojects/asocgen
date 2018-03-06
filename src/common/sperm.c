#include "sperm.h"

#include "sarray16.h"

void binom_init(Array_uint16 *array, uint16_t n) {
  aui16_fill(array, 0xffff);
  aui16_setToRange(array, 0, n, 0);
}

void perm_init(Array_uint16 *array) {
  aui16_setToRange(array, 0, array->size, 0);
}

/*
  since the binom can be 0xffff terminated, more complicated cases need to be
  considered (ie. is t0 end element / is t1 end element)
*/
bool binom_shiftDefault(Array_uint16 *array, uint16_t max) {
  uint32_t n = 0;
  uint32_t i;
  for(i = 0; i < array->size; i++) {
    if(*aui16_at(array, i) == 0xffff) {
      break;
    } else {
      n++;
    }
  }
  return binom_shift(array, 0, max, 0, n);
}

bool binom_shift(Array_uint16 *array, uint16_t min, uint16_t max,
                 uint16_t offset, uint16_t n) {
  uint32_t i;
  uint16_t *ele_i;
  uint16_t *ele_im1;
  for(i = offset + 1; i < offset + n; i++) {
    ele_i = aui16_at(array, i);
    ele_im1 = aui16_at(array, i - 1);
    if(*ele_i - *ele_im1 > 1) { // inc i - 1 if possible
      (*ele_im1)++;
      aui16_setToRange(array, offset, i - 1, min); // reset before i - 1
      return !(i == offset + 1 && *ele_im1 == max - n + 1);
    }
    if(i == offset + n - 1 && *ele_i < max) { // inc last ele if possible?
      (*ele_i)++;
      aui16_setToRange(array, offset, i, min); // reset before last ele
      return 1;
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
  return array->size;
}

uint16_t nextBiggestInd(Array_uint16 *array, uint32_t pos, uint32_t to) {
  uint16_t curr = *aui16_at(array, pos);
  uint16_t cand = 0xffff;
  uint16_t ind = 0xffff;
  uint32_t i;
  uint16_t ele;
  for(i = pos + 1; i < to; i++) {
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

bool perm_shiftDefault(Array_uint16 *array) {
  return perm_shift(array, 0, array->si
}

bool perm_shift(Array_uint16 *array, uint16_t from, uint16_t to) {
  int32_t pos = to - 2;
  uint16_t nbi;
  while(pos >= from) {
    nbi = nextBiggestInd(array, pos, to);
    if(nbi != 0xffff) {
      switchElements(array, pos, nbi);
      aui16_sort(array, pos + 1, to - 1);
      return 1;
    } else {
      pos--;
    }
  }
  return 0;
}

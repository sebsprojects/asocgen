#include <stdio.h>
#include "asocgen.h"


Zip allocZip(uint8_t n) {
  Zip zip;
  zip.n = n;
  zip.pos = 1;
  zip.lastEntry = 0;
  zip.iord = allocArray_uint8((n - 1) * (n - 1));
  zip.candidates = allocArray_uint8(n);
  return zip;
}

void freeZip(Zip *zip) {
  freeArray_uint8(zip->iord);
  freeArray_uint8(zip->candidates);
}

uint8_t getMTabIndexAtPos(Zip *zip) {
  return *at_uint8(zip->iord, zip->pos);
}

uint8_t getMTabIndexAtLast(Zip *zip) {
  return *at_uint8(zip->iord, zip->lastEntry);
}

bool isOverEnd(Zip *zip) {
  return zip->pos >= (zip->n - 1) * (zip->n - 1);
}

bool isOverStart(Zip *zip) {
  return zip->pos < 0;
}

bool doStep(MTab *mtab, Zip* zip, Info* info) {
  if(isOverEnd(zip)) {
    //printMTab(zip->n, mtab);
    info->numGroups++;
    zip->pos--;
  }

  if(!upEntry(mtab, zip)) return 0;

  if(!isAsocIncmplIncrm(mtab, zip)) {
    if(zip->pos > zip->lastEntry) {
      zip->pos--;
    }
  }
  return 1;
}

bool upEntry(MTab *mtab, Zip *zip) {
  uint8_t n = zip->n;
  uint32_t index = getMTabIndexAtPos(zip);
  uint8_t r = getRowIndex(n, index);
  uint8_t c = getColIndex(n, index);
  uint8_t currentEle = *at_uint8(mtab, index);

  // ele = max ele
  if(currentEle == n - 1) {
    *at_uint8(mtab, index) = 0xff;
    // Are we at the beginning?
    if(zip->pos == 0) {
      return 0;
    } else {
      zip->pos--;
      zip->lastEntry--;
      return 1;
    }
  }
  uint8_t offset;
  if(currentEle == 0xff) {
    offset = 0;
  } else {
    offset = currentEle + 1; // in [1, n - 1]
  }
  uint8_t ele;
  uint32_t i;
  for(i = 0; i < n - offset; i++) {
    *at_uint8(zip->candidates, i) = i + offset;
  }
  // check rows
  for(i = r * n; i < (r + 1) * n; i++) {
    ele = *at_uint8(mtab, i);
    if(ele != 0xff && (currentEle == 0xff || ele > currentEle)) {
      *at_uint8(zip->candidates, ele - offset) = 0xff;
    }
  }
  // check cols
  for(i = c; i < n * (n - 1) + c; i+=n) {
    ele = *at_uint8(mtab, i);
    if(ele != 0xff && (currentEle == 0xff || ele > currentEle)) {
      *at_uint8(zip->candidates, ele - offset) = 0xff;
    }
  }
  //printArray_uint8(pstring, zip->candidates);
  uint8_t nextB = 0xff;
  for(i = 0; i < n - offset; i++) {
    ele = *at_uint8(zip->candidates, i);
    if(ele != 0xff) {
      nextB = ele;
      break;
    }
  }
  if(nextB == 0xff) {
    *at_uint8(mtab, index) = 0xff;
    zip->pos--;
    if(currentEle != 0xff) zip->lastEntry--;
  } else {
    *at_uint8(mtab, index) = nextB;
    zip->pos++;
    if(currentEle == 0xff) zip->lastEntry++;
  }
  return 1;
}


bool isAsocIncmplNaive(MTab *mtab, Zip* zip) {
  uint8_t a, b, c, ab, ab_c, bc, a_bc;
  uint8_t n = zip->n;

  for(a = 0; a < n; a++) {
    for(b = 0; b < n; b++) {
      for(c = 0; c < n; c++) {
	ab = *at_uint8(mtab, getMTabIndex(n, a, b));
	if(ab == 0xff) continue;
	bc = *at_uint8(mtab, getMTabIndex(n, b, c));
	if(bc == 0xff) continue;
	ab_c = *at_uint8(mtab, getMTabIndex(n, ab, c));
	if(ab_c == 0xff) continue;
	a_bc = *at_uint8(mtab, getMTabIndex(n, a, bc));
	if(a_bc == 0xff) continue;
	if(ab_c != a_bc) return 0;
      }
    }
  }
  return 1;
}

bool isAsocIncmplIncrm(MTab *mtab, Zip* zip) {
  uint8_t n = zip->n;
  uint32_t index = getMTabIndexAtLast(zip);
  uint8_t a = getRowIndex(n, index);
  uint8_t b = getColIndex(n, index);
  uint8_t ab = *at_uint8(mtab, index);

  // check ab_c = a_bc
  uint8_t c, bc, ab_c, a_bc;
  for(c = 0; c < n; c++) {
    bc = *at_uint8(mtab, getMTabIndex(n, b, c));
    if(bc == 0xff) continue;
    a_bc = *at_uint8(mtab, getMTabIndex(n, a, bc));
    if(a_bc == 0xff) continue;
    ab_c = *at_uint8(mtab, getMTabIndex(n, ab, c));
    if(ab_c == 0xff) continue;
    if(ab_c != a_bc) return 0;
  }

  // check c_ab = ca_b
  uint8_t ca, c_ab, ca_b;
  for(c = 0; c < n; c++) {
    ca = *at_uint8(mtab, getMTabIndex(n, c, a));
    if(ca == 0xff) continue;
    ca_b = *at_uint8(mtab, getMTabIndex(n, ca, b));
    if(ca_b == 0xff) continue;
    c_ab = *at_uint8(mtab, getMTabIndex(n, c, ab));
    if(c_ab == 0xff) continue;
    if(ca_b != c_ab) return 0;
  }

  // go through preimages of a to check (x)(y b) = [(x y) b = ab]
  uint8_t ele, x, y, yb, x_yb;
  uint32_t i;
  for(i = 0; i < mtab->size; i++) {
    ele = *at_uint8(mtab, i);
    if(ele == a) {
      x = getRowIndex(n, i);
      y = getColIndex(n, i);
      yb = *at_uint8(mtab, getMTabIndex(n, y, b));
      if(yb == 0xff) continue;
      x_yb = *at_uint8(mtab, getMTabIndex(n, x, yb));
      if(x_yb == 0xff) continue;
      if(x_yb != ab) return 0;
    }
  }

  // go through preimages of b to check (a x) y = [a (x y) = ab
  uint8_t ax, ax_y;
  for(i = 0; i < mtab->size; i++) {
    ele = *at_uint8(mtab, i);
    if(ele == b) {
      x = getRowIndex(n, i);
      y = getColIndex(n, i);
      ax = *at_uint8(mtab, getMTabIndex(n, a, x));
      if(ax == 0xff) continue;
      ax_y = *at_uint8(mtab, getMTabIndex(n, ax, y));
      if(ax_y == 0xff) continue;
      if(ax_y != ab) return 0;
    }
  }
  return 1;
}

void initIOrd(Zip *zip) {
  uint8_t pos = 0;
  uint8_t val = 0;
  uint8_t n = zip->n;
  uint8_t i, r, d;

  for(i = 1; i < n; i++) {
    val = i * n + 1;
    *at_uint8(zip->iord, pos) = val;
    pos++;
    for(r = 0; r < i - 1; r++) {
      val++;
      *at_uint8(zip->iord, pos) = val;
      pos++;
    }
    for(d = 0; d < i - 1; d++) {
      val -= n;
      *at_uint8(zip->iord, pos) = val;
      pos++;
    }
  }
}

void initMTab(MTab* mtab, Zip *zip, uint8_t init) {
  uint8_t i;
  uint8_t j;
  uint8_t n = zip->n;
  for(i = 0; i < n; i++) {
    for(j = 0; j < n; j++) {
      if(i == 0) {
	*at_uint8(mtab, j) = j;
      } else if(j == 0) {
	*at_uint8(mtab, i * n) = i;
      } else {
	*at_uint8(mtab, i * n + j) = 0xff;
      }
    }
  }
  *at_uint8(mtab, n + 1) = init;
}

void printMTab(char *pstring, MTab *mtab, uint8_t n) {
  sprintArraySquare_uint8(pstring, mtab, n);
  printf("%s", pstring);
}

void printZip(Zip *zip) {
  printf("Zip = [ n=%u p=%u l=%u ]\n", zip->n, zip->pos, zip->lastEntry);
}

void printIOrd(char *pstring, Zip *zip) {
  sprintArray_uint8(pstring, zip->iord);
  printf("%s", pstring);
}

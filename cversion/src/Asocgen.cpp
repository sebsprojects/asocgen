// I hate C so much but I need to be sure ...

#include <iostream>
#include <sstream>
#include <vector>

#include "Asocgen.h"


bool doStep(std::vector<int>* mtab, Zip* zip, Info* info) {
  //std::cout << "last entry: " << zip->lastEntry << std::endl;
  //std::cout << std::endl;

  if(zip->isOverEnd()) {
    printMTab(zip->n, mtab);
    std::cout << std::endl;
    info->num_groups++;
    zip->pos--;
  }

  upEntry(mtab, zip);

  if(zip->isOverStart()) {
    return false;
  }

  if(!isAsocIncmplIncrm(mtab, zip)) {
    if(zip->pos > zip->lastEntry) {
      zip->pos--;
    }
    //std::cout << "Not asoc!" << std::endl;
    //printMTab(mtab);
    //std::cout << std::endl;
  }
  //printMTab(mtab);
  //std::cout << std::endl;
  return true;
}

bool isAsocIncmplNaive(std::vector<int>* mtab, Zip* zip) {
  int ab = -2;
  int ab_c = -2;
  int bc = -2;
  int a_bc = -2;
  int n = zip->n;
  for(int a = 0; a < n; a++) {
    for(int b = 0; b < n; b++) {
      for(int c = 0; c < n; c++) {
	ab = mtab->at(getMTabIndex(n, a, b));
	if(ab < 0) continue;
	bc = mtab->at(getMTabIndex(n, b, c));
	if(bc < 0) continue;
	ab_c = mtab->at(getMTabIndex(n, ab, c));
	if(ab_c < 0) continue;
	a_bc = mtab->at(getMTabIndex(n, a, bc));
	if(a_bc < 0) continue;
	if(ab_c != a_bc) return false;
      }
    }
  }
  return true;
}

bool isAsocIncmplIncrm(std::vector<int>* mtab, Zip* zip) {
  int n = zip->n;
  int index = zip->getMTabIndexAtLast();
  int a = getRowIndex(n, index);
  int b = getColIndex(n, index);
  int ab = mtab->at(index);

  // check ab_c = a_bc
  int bc = -2;
  int ab_c = -2;
  int a_bc = -2;
  for(int c = 0; c < n; c++) {
    bc = mtab->at(getMTabIndex(n, b, c));
    if(bc < 0) continue;
    a_bc = mtab->at(getMTabIndex(n, a, bc));
    if(a_bc < 0) continue;
    ab_c = mtab->at(getMTabIndex(n, ab, c));
    if(ab_c < 0) continue;
    if(ab_c != a_bc) return false;
  }

  // check c_ab = ca_b
  int ca = -2;
  int c_ab = -2;
  int ca_b = -2;
  for(int c = 0; c < n; c++) {
    ca = mtab->at(getMTabIndex(n, c, a));
    if(ca < 0) continue;
    ca_b = mtab->at(getMTabIndex(n, ca, b));
    if(ca_b < 0) continue;
    c_ab = mtab->at(getMTabIndex(n, c, ab));
    if(c_ab < 0) continue;
    if(ca_b != c_ab) return false;
  }
  // go through preimages of a to check (x)(y b) = [(x y) b = ab]
  int ele = -2;
  int x = -2;
  int y = -2;
  int yb = -2;
  int x_yb = -2;
  for(size_t i = 0; i < mtab->size(); i++) {
    ele = mtab->at(i);
    if(ele == a) {
      x = getRowIndex(n, i);
      y = getColIndex(n, i);
      yb = mtab->at(getMTabIndex(n, y, b));
      if(yb < 0) continue;
      x_yb = mtab->at(getMTabIndex(n, x, yb));
      if(x_yb < 0) continue;
      if(x_yb != ab) return false;
    }
  }

  // go through preimages of b to check (a x) y = [a (x y) = ab
  int ax = -2;
  int ax_y = -2;
  for(size_t i = 0; i < mtab->size(); i++) {
    ele = mtab->at(i);
    if(ele == b) {
      x = getRowIndex(n, i);
      y = getColIndex(n, i);
      ax = mtab->at(getMTabIndex(n, a, x));
      if(ax < 0) continue;
      ax_y = mtab->at(getMTabIndex(n, ax, y));
      if(ax_y < 0) continue;
      if(ax_y != ab) return false;
    }
  }
  return true;
}

void upEntry(std::vector<int>* mtab, Zip* zip) {
  int n = zip->n;
  int index = zip->getMTabIndexAtPos();
  int r = getRowIndex(n, index);
  int c = getColIndex(n, index);
  int currentEle = mtab->at(index);

  //std::cout << "Before upEntry:" << std::endl;
  //printMTab(mtab);
  //std::cout << zip->toString() << std::endl;
  //std::cout << "r=" << r << " c=" << c << std::endl;

  if(currentEle == n - 1) {
    mtab->at(index) = -1;
    zip->pos--;
    zip->lastEntry--;
    return;
  }

  int offset = currentEle + 1;
  int ele = -1;
  std::vector<int> candidates(n - offset, 0);
  for(size_t i = 0; i < candidates.size(); i++) {
    candidates.at(i) = i + offset;
  }
  for(int i = r * n; i < (r + 1) * n; i++) {
    ele = mtab->at(i);
    if(ele > currentEle) candidates.at(ele - offset) = -1;
  }
  for(int i = c; i < n * (n - 1) + c; i+=n) {
    ele = mtab->at(i);
    if(ele > currentEle) candidates.at(ele - offset) = -1;
  }

  int nextB = -1;
  for(size_t i = 0; i < candidates.size(); i++) {
    ele = candidates.at(i);
    if(ele > -1) {
      nextB = ele;
      break;
    }
  }

  if(nextB == -1) {
    mtab->at(index) = -1;
    zip->pos--;
    if(currentEle != -1) zip->lastEntry--;
  } else {
    mtab->at(index) = nextB;
    zip->pos++;
    if(currentEle == -1) zip->lastEntry++;
  }

  //std::cout << "After upEntry:" << std::endl;
  //printMTab(mtab);
  //std::cout << zip->toString() << std::endl << std::endl;
  //std::cout << "====================================" << std::endl;
}

void initIOrd(int n, std::vector<int>* iord) {
  int pos = 0;
  int val = 0;
  for(int i = 1; i < n; i++) {
    val = i * n + 1;
    iord->at(pos) = val;
    pos++;
    for(int r = 0; r < i - 1; r++) {
      val++;
      iord->at(pos) = val;
      pos++;
    }
    for(int d = 0; d < i - 1; d++) {
      val -= n;
      iord->at(pos) = val;
      pos++;
    }
  }
}

void initMTab(int n, std::vector<int>* mtab, int init) {
  for(int i = 0; i < n; i++) {
    mtab->at(i) = i;
    mtab->at(i * n) = i;
  }
  mtab->at(n + 1) = init;
}

int getRowIndex(int n, int index) {
  return index / n;
}

int getColIndex(int n, int index) {
  return index % n;
}

int getMTabIndex(int n, int row, int col) {
  return row * n + col;
}

void printVector(std::vector<int>* vec) {
  std::stringstream ss;
  ss << "[ ";
  for(size_t i = 0; i < vec->size(); i++) {
    ss << vec->at(i);
    if(i != vec->size() - 1) ss << ", ";
    else ss << " ]";
  }
  std::cout << ss.str() << std::endl;
}

void printMTab(int n, std::vector<int>* mtab) {
  int ele = -2;
  std::stringstream ss;
  for(int r = 0; r < n; r++) {
    for(int c = 0; c < n; c++) {
      ele = mtab->at(r * n + c);
      if(ele < 0 || ele > 9) {
	ss << " ";
      } else {
	ss << "  ";
      }
      ss << ele;
    }
    ss << std::endl;
  }
  std::cout << ss.str(); //<< std::endl;
}

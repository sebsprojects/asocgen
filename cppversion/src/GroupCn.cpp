#include "GroupCn.h"

GroupCn::GroupCn(int n) : Group<int>() {
  for(int i = 0; i < n; i++) {
    set.push_back(i);
  }
}

GroupCn::~GroupCn() {

}

int GroupCn::op(int a, int b) const {
  return evalMTab(a, b);
}

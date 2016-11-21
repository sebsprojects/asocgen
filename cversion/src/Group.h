#ifndef GROUP
#define GROUP

#include <vector>

template <class V>
class Group {

public:

  int order;
  std::vector<V> set;
  std::vector<V> mtab;

  virtual V op(V a, V b) const {
    int ind1 = -1;
    int ind2 = -1;
    int i = 0;
    while(ind1 < 0 || ind2 < 0) {
      if(set.at(i) == a) ind1 = i;
      if(set.at(i) == b) ind2 = i;
      i++;
    }
    return evalMTab(ind1, ind2);
  }

  virtual V evalMTab(int r, int c) const {
    return mtab.at(r * order + c);
  }

protected:
  V one;
  std::vector<V> invs;

  bool valid;

};

#endif

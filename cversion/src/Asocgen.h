#ifndef ASOCGEN
#define ASOCGEN

#include <sstream>
#include <string>
#include <vector>

class Zip;
class Info;

void printMTab(int n, std::vector<int>* mtab);
void printVector(std::vector<int>* vec);

void initIOrd(int n, std::vector<int>* iord);
void initMTab(int n, std::vector<int>* mtab, int init = 0);

void upEntry(std::vector<int>* mtab, Zip* zip);
bool doStep(std::vector<int>* mtab, Zip* zip, Info* info);

bool isAsocIncmplNaive(std::vector<int>*, Zip* zip);
bool isAsocIncmplIncrm(std::vector<int>*, Zip* zip);

inline int getRowIndex(int n, int row);
inline int getColIndex(int n, int col);

inline int getMTabIndex(int n, int row, int col);


class Zip {

 public:
  int n;
  int pos;
  int lastEntry;
  std::vector<int> iord;

  Zip(int n, std::vector<int> iordv, int p = 1) :
    n(n), pos(p), lastEntry(p - 1), iord(iordv)
  {
  };
  ~Zip() { };

  int getMTabIndexAtPos() { return iord.at(pos); }
  int getMTabIndexAtLast() { return iord.at(lastEntry); }
  bool isOverEnd() { return pos >= static_cast<int>(iord.size()); }
  bool isOverStart() { return pos < 0; }

  std::string toString() {
    std::stringstream ss;
    ss << "Z=[ " << pos << " " << lastEntry << " ]";
    return ss.str();
  }
};


class Info {

public:
  int num_groups;

  Info() : num_groups(0) { };
  ~Info() { };

};

#endif

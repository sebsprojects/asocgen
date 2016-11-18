#ifndef ASOCGEN
#define ASOCGEN

#include <sstream>
#include <string>
#include <vector>

class Zip;

void printMTab(std::vector<int>* mtab);
void printVector(std::vector<int>* vec);

void initIOrd(std::vector<int>* iord);
void initMTab(std::vector<int>* mtab, int init = 0);

void upEntry(std::vector<int>* mtab, Zip* zip);
bool doStep(std::vector<int>* mtab, Zip* zip);

bool isAsocIncmplNaive(std::vector<int>*, Zip* zip);
bool isAsocIncmplIncrm(std::vector<int>*, Zip* zip);

int getRowIndex(int row);
int getColIndex(int col);

inline int getMTabIndex(int row, int col);


class Zip {

 public:
  int pos;
  int lastEntry;
  std::vector<int> iord;

  Zip(std::vector<int> iordv, int p = 1) :
    pos(p), lastEntry(p - 1), iord(iordv)
  {
  };

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
#endif

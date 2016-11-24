#include <vector>
#include <iostream>

#include "Asocgen.h"

int main() {
  int n = 8;

  // Init
  std::vector<int> mtab(n * n, -1);
  std::vector<int> iord((n - 1) * (n - 1), -1);

  initIOrd(n, &iord);
  initMTab(n, &mtab);

  Zip zip(n, iord);
  Info info;

  bool notDone = true;
  while(notDone) {
    notDone = doStep(&mtab, &zip, &info);
  }
  std::cout << "Total num groups: " << info.num_groups << std::endl;
}

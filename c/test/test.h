#ifndef ASOC_TEST
#define ASOC_TEST

#include "../src/common/common.h"

inline void printTestHead(char *modName, char *funName) {
  printf("\n>> %s :: %s ...\n", modName, funName);
}

inline void printTestFoot(bool hasPassed) {
  printf("<< result: %u\n", hasPassed);
}

#endif

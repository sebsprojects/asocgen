#ifndef ASOCGEN
#define ASOCGEN

#include <stdint.h>
#include "../common/common_includes.h"


typedef Array_uint8 MTab;

struct Zip {
  uint8_t n;
  uint32_t pos;
  uint32_t lastEntry;
  Array_uint8 *iord;
  Array_uint8 *candidates;
};
typedef struct Zip Zip;

Zip *allocZip(uint8_t n);
void freeZip(Zip *zip);

uint8_t getMTabIndexAtPos(Zip *zip);
uint8_t getMTabIndexAtLast(Zip *zip);
bool isOverEnd(Zip *zip);
bool isOverStart(Zip *zip);

void initIOrd(Zip *zip);
void initMTab(MTab* mtab, Zip *zip, uint8_t init);

bool upEntry(MTab *mtab, Zip* zip);
bool doStep(MTab *mtab, Zip* zip);

bool isAsocIncmplNaive(MTab *mtab, Zip* zip);
bool isAsocIncmplIncrm(MTab *mtab, Zip* zip);

void printZip(Zip *zip);

bool isComplete(MTab *mtab, uint8_t n);

#endif

#ifndef ASOCGEN
#define ASOCGEN

#include <stdint.h>

#include "../common/common.h"


typedef Array_uint8 MTab;

struct Zip {

uint8_t n;
uint32_t pos;
uint32_t lastEntry;
Array_uint8 *iord;
Array_uint8 *candidates;

};
typedef struct Zip Zip;

struct Info { uint32_t numGroups; };
typedef struct Info Info;

Zip allocZip(uint8_t n);
void freeZip(Zip *zip);

uint8_t getMTabIndexAtPos(Zip *zip);
uint8_t getMTabIndexAtLast(Zip *zip);
bool isOverEnd(Zip *zip);
bool isOverStart(Zip *zip);

void initIOrd(Zip *zip);
void initMTab(MTab* mtab, Zip *zip, uint8_t init);

bool upEntry(MTab *mtab, Zip* zip);
bool doStep(MTab *mtab, Zip* zip, Info* info);

bool isAsocIncmplNaive(MTab *mtab, Zip* zip);
bool isAsocIncmplIncrm(MTab *mtab, Zip* zip);

void printMTab(char *pstring, MTab *mtab, uint8_t n);
void printZip(Zip *zip);
void printIOrd(char *pstring, Zip *zip);

inline uint8_t getRowIndex(uint8_t n, uint32_t ind) {
  return ind / n;
}

inline uint8_t getColIndex(uint8_t n, uint32_t ind) {
  return ind % n;
}

inline uint32_t getMTabIndex(uint8_t n, uint8_t row, uint8_t col) {
  return row * n + col;
}

#endif

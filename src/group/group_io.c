#include "group_io.h"

#include <elfc_hash.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

static const f64 baseLog16 = 1.0 / log(16.0);
static const u32 lineLength = 80;


void group_writeToFile(Group *group)
{
  // TODO: Makes sure group is indexed
  u16 n = group_order(group);
  u16 maxEleLen = floor(log((f64) n) * baseLog16) + 1;
  u16 elePerLine = lineLength / maxEleLen;
  u16 numLines = 1 + (n * n / elePerLine);
  u16 ele = 0;
  char formatString[4];
  formatString[0] = '%';
  formatString[1] = maxEleLen + '0';
  formatString[2] = 'x';
  formatString[3] = '\0';
  char *fbuf = malloc(n * n * maxEleLen + numLines + 100); // \n * numLines
  char *hbuf = malloc(n * n * maxEleLen + 100);
  fbuf[0] = '\0';
  hbuf[0] = '\0';
  u32 foffs = strlen(fbuf);
  u32 hoffs = strlen(hbuf);
  for(i32 i = 0; i < n * n; i++) {
    ele = *vecu16_at(group->gtab, i);
    foffs += sprintf(fbuf + foffs, formatString, ele);
    hoffs += sprintf(hbuf + hoffs, "%x", ele);
    if(i % elePerLine == elePerLine - 1) {
      foffs += sprintf(fbuf + foffs, "\n");
    }
  }
  if((n * n) % elePerLine != 0) {
    sprintf(fbuf + foffs, "\n"); // make sure we \n terminate the string
  }
  printf("%s\n", fbuf);
  printf("fbuf=%lu :: hbuf=%lu\n", strlen(fbuf), strlen(hbuf));
  u64 hash = hash_djb2Reverse(hbuf);
  char fileName[16 + 1 + 5 + 100]; // 16 char hash + _ + group_order + buffer
  fileName[0] = '\0';
  foffs = sprintf(fileName, "%05i_", n);
  foffs += hash_sprintHash(fileName + foffs, hash);
  sprintf(fileName + foffs, ".txt");
  printf("%s\n", fileName);
  free(fbuf);
  free(hbuf);
}

void group_readFromFile(Group *group)
{

}

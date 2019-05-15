#include "group_io.h"

#include <elfc_hash.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>


static const f64 baseLog16 = 1.0 / log(16.0);


bool group_writeToFile(Group *group, char *path)
{
  if(group->indexed) {
    return group_writeIndexedToFile(group, path);
  } else {
    Group *g = group_getIndexedCopy_alloc(group);
    bool ok = group_writeIndexedToFile(g, path);
    group_free(g);
    return ok;
  }
}

bool group_writeIndexedToFile(Group *group, char *path)
{
  return 0;
}

i32 group_sprintGTab(char *buf, Group *group)
{
  u16 n = group_order(group);
  u16 maxEleLen = floor(log((f64) n) * baseLog16) + 1;
  char formatString[4];
  formatString[0] = '%';
  formatString[1] = maxEleLen + '0';
  formatString[2] = 'x';
  formatString[3] = '\0';
  u32 offs = strlen(buf);
  u16 ele = 0;
  for(i32 i = 0; i < n * n; i++) {
    ele = *vecu16_at(group->gtab, i);
    offs += sprintf(buf + offs, formatString, ele); // %[maxEleLen]x
  }
  return offs;
}

i32 group_sprintHash(char *buf, char *gtabBuf)
{
  u64 hash = hash_djb2(gtabBuf);
  return hash_sprintHash(buf, hash);
}

i32 group_sprintFileName(char *buf, Group* group, char *gtabBuf)
{
  u32 offs = sprintf(buf, "%05i_", group_order(group));
  offs += group_sprintHash(buf + offs, gtabBuf);
  offs += sprintf(buf + offs, ".txt");
  return offs;
}

i32 group_sprintPreamble(char *buf)
{
  return 0;
}

void group_readFromFile(char *path)
{
}

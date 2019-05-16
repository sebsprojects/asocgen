#include "group_io.h"

#include <elfc_hash.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>


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

i32 group_sprintFileName(char *buf, Group* group, u64 hash)
{
  u32 offs = 0;
  char commFlag = group_isCommutative(group) ? 'c' : 'n';
  offs += sprintf(buf + offs, "%05i%c_", group_order(group), commFlag);
  offs += hash_sprintHash(buf + offs, hash);
  offs += sprintf(buf + offs, ".txt");
  return offs;
}

i32 group_sprintHeader(char *buf, GroupMetaInfo meta)
{
  i32 offs = group_sprintPreamble(buf);
  offs += sprintf(buf + offs, "\n#\n");
  offs += group_sprintFileMeta(buf + offs);
  offs += sprintf(buf + offs, "\n#\n");
  offs += group_sprintGroupMeta(buf + offs, meta);
  offs += sprintf(buf + offs, "\n#\n#\n#");
  return offs;
}

i32 group_sprintPreamble(char *buf)
{
  return sprintf(buf, "# This file is generated by ascogen and contains the"
                      " multiplication table of a\n"
                      "# finite group. For more information please visit"
                      " https://elfeck.com/asocgen.");
}

i32 group_sprintFileMeta(char *buf)
{
  time_t t = time(0);
  struct tm tm = *localtime(&t);
  i32 offs = 0;
  offs += sprintf(buf + offs, "# Version: %s\n", VERSION);
  offs += sprintf(buf + offs, "# File Created: %d-%d-%d", tm.tm_year + 1900,
                  tm.tm_mon + 1, tm.tm_mday);
  return offs;
}

i32 group_sprintGroupMeta(char *buf, GroupMetaInfo meta)
{
  i32 offs = 0;
  offs += sprintf(buf + offs, "# Group Name: %s\n", meta.name);
  offs += sprintf(buf + offs, "# Group djb2-Hash: ");
  offs += hash_sprintHash(buf + offs, meta.djb2Hash);
  offs += sprintf(buf + offs, "\n");
  offs += sprintf(buf + offs, "# Group Order: %i\n", meta.order);
  offs += sprintf(buf + offs, "# Group Commutative: %i\n", meta.isCommutative);
  offs += sprintf(buf + offs, "# Group minGenSet: ");
  return offs;
}


// ---------------------------------------------------------------------------
// Reading
// ---------------------------------------------------------------------------

void group_readFromFile(char *path)
{
}

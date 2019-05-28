#include "group_io.h"

#include <elfc_hash.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>


static const f64 baseLog16 = 1.0 / log(16.0);


bool group_writeToFile(Group *group, GroupMetaInfo meta, char *path)
{
  if(group->indexed) {
    return group_writeIndexedToFile(group, meta, path);
  } else {
    Group *g = group_getIndexedCopy_alloc(group);
    bool ok = group_writeIndexedToFile(g, meta, path);
    group_free(g);
    return ok;
  }
}

bool group_writeIndexedToFile(Group *group, GroupMetaInfo meta, char *path)
{
  u16 n = group_order(group);
  u16 maxEleLen = floor(log((f64) n) * baseLog16) + 1;
  char *fnameBuf = malloc(strlen(path) + 100);
  char *contentBuf = malloc(n * n * maxEleLen + 100);
  contentBuf[0] = '\0';
  fnameBuf[0] = '\0';
  group_sprintGTab(contentBuf, group);
  u64 hash = hash_djb2(contentBuf);
  strcpy(fnameBuf, path);
  i32 offs = strlen(fnameBuf);
  offs += sprintf(fnameBuf + offs, "/");
  group_sprintFileName(fnameBuf + offs, group, hash);
  printf("%s\n", fnameBuf);
  FILE *f = 0;
  if((f = fopen(fnameBuf, "r")) != 0) { // check if file exists
    fclose(f);
    free(contentBuf);
    free(fnameBuf);
    return 0;
  }
  if((f = fopen(fnameBuf, "w")) == 0) { // check if we could open file for wr
    free(contentBuf);
    free(fnameBuf);
    return 0;
  }
  char *headerBuf = malloc(1000); // TODO: Improve this value
  headerBuf[0] = '\0';
  group_sprintHeader(headerBuf, meta);
  fprintf(f, "%s\n%s\n", headerBuf, contentBuf);
  fclose(f);
  free(contentBuf);
  free(fnameBuf);
  free(headerBuf);
  return 1;
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
  if(meta.minGenSet != 0) {
    Vecu16 *gen = meta.minGenSet;
    for(i32 i = 0; i < gen->size - 1; i++) {
      offs += sprintf(buf + offs, "%x, ", *vecu16_at(gen, i));
    }
    offs += sprintf(buf + offs, "%x", *vecu16_at(gen, gen->size - 1));
  }
  return offs;
}


// ---------------------------------------------------------------------------
// Reading
// ---------------------------------------------------------------------------

Group *group_readGroupFromFile_alloc(char *path)
{
  FILE *f = 0;
  if((f = fopen(path, "r")) == 0) { // check if file exists
    return 0;
  }
  char *fileName = strchr(path, '/') + 1;
  u16 order = 0xffff;
  sscanf(fileName, "%5hu", &order);
  char c = fgetc(f);
  i64 startPos = -1;
  if(c == EOF) {
    fclose(f);
    return 0;
  }
  while(c == '#') {
    c = fgetc(f);
    while(c != '\n') {
      c = fgetc(f);
    }
    startPos = ftell(f);
    c = fgetc(f);
  }
  if(c == EOF) {
    fclose(f);
    return 0;
  }
  i32 entryLen = floor(log((f64) order) * baseLog16) + 1;
  // Alternative: Get the table-entry char-length by counting spaces up to the
  // first entry which should be 0
  //while(c == ' ') {
  //  entryLen++;
  //  c = fgetc(f);
  //}
  fseek(f, startPos, SEEK_SET);
  char stringToken[5];
  Group *group = group_alloc(order, 1);
  vecu16_setToRange(group->set, 0, order, 0);
  u16 entry = 0xffff;
  //TODO: This could probably done by fscanf but I could not make it work with
  //the whitespace and non-delimiter formatting
  u32 count = 0;
  while(!feof(f)) {
    fgets(stringToken, entryLen + 1, f);
    if(stringToken[0] == '\n') {
      break;
    }
    sscanf(stringToken, "%hx", &entry);
    *vecu16_at(group->gtab, count) = entry;
    count++;
    //printf("@%u/%u :: Read :: String :%s: :: Entry :%u:\n",
    //       count, order * order, stringToken, entry);
  }
  fclose(f);
  return group;
}

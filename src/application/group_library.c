#include "group_library.h"

#include "../group/group_io.h"
#include "../group/group_gen.h"
#include "../group/group_common.h"

#include <elfc_random.h>

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <dirent.h>


// ---------------------------------------------------------------------------
// Writing constructable groups
// ---------------------------------------------------------------------------

//TODO: Maybe get an indexed copy before making expensive calculations
bool app_writeGroup(char *path, Group *group, char *name)
{
  GroupMetaInfo meta;
  memset(meta.name, '\0', GROUP_META_NAME_LEN); // avoid garbage in meta.name
  memset(meta.minGenSet, 0xffff, GROUP_META_MINGENSET_LEN * sizeof(u16));
  strcpy(meta.name, name);
  meta.order = group_order(group);
  meta.isCommutative = group_isCommutative(group);
  Vecu16 *minGenSet = group_minGeneratingSet_alloc(group);
  group_truncGeneratedSet(minGenSet, 1);
  meta.minGenSetSize = minGenSet->size;
  vecu16_copyIntoArray(meta.minGenSet, minGenSet, minGenSet->size);
  vecu16_free(minGenSet);
  return group_writeToFile(group, meta, path);
}

bool app_writeGroupSn(char *path, u32 n)
{
  char name[16];
  memset(name, 0, 16);
  sprintf(name, "S%u", n);
  Group *group = group_createSn_alloc(n);
  bool ok = app_writeGroup(path, group, name);
  group_free(group);
  return ok;
}

bool app_writeGroupCn(char *path, u32 n)
{
  char name[16];
  memset(name, 0, 16);
  sprintf(name, "C%u", n);
  Group *group = group_createCn_alloc(n);
  bool ok = app_writeGroup(path, group, name);
  group_free(group);
  return ok;
}


// ---------------------------------------------------------------------------
// Read group files
// ---------------------------------------------------------------------------

Vecptr *app_listGroupFiles_alloc(char *path, u16 order, i32 isCommutative)
{
  DIR *dir = opendir(path);
  if(dir == 0) {
    return 0;
  }
  struct dirent *ent = 0;
  u16 fOrder = 0;
  bool fIsComm = 0;
  // Get the count of files fullfilling the requirements
  u32 count = 0;
  while((ent = readdir(dir)) != 0) {
    bool validFile = group_checkGroupFileName(ent->d_name, &fOrder, &fIsComm);
    validFile = validFile && ((order <= 0) || order == fOrder);
    validFile = validFile && ((isCommutative < 0) || isCommutative == fIsComm);
    if(validFile) {
      count++;
    }
  }
  rewinddir(dir);
  Vecptr *fileNames = vecptr_alloc(count);
  u32 ts = *(strrchr(path, '\0') - 1) == '/' ? 0 : 1;
  u32 totalPathLen = strlen(path) + ts + GROUP_META_FILENAME_LEN;
  i32 ind = 0;
  while((ent = readdir(dir)) != 0) {
    // also makes sure d_name is exactly GROUP_META_FILENAME_LEN long
    bool validFile = group_checkGroupFileName(ent->d_name, &fOrder, &fIsComm);
    validFile = validFile && ((order <= 0) || order == fOrder);
    validFile = validFile && ((isCommutative < 0) || isCommutative == fIsComm);
    if(validFile) {
      char *fileName = malloc(totalPathLen + 1);
      memset(fileName, '\0', totalPathLen + 1);
      strcat(fileName, path);
      if(ts == 1) {
        strcat(fileName, "/");
      }
      strcat(fileName, ent->d_name);
      *vecptr_at(fileNames, ind) = fileName;
      ind++;
    }
  }
  vecptr_resize(fileNames, ind);
  closedir(dir);
  return fileNames;
}


// --------------------------------------------------------------------------
// Search for groups
// --------------------------------------------------------------------------

Group *app_searchForGroup_alloc(Group *parent,
                                u16 order, i32 isCommutative,
                                u32 genSizeLower, u32 genSizeUpper,
                                u32 iter)
{
  u16 n = group_order(parent);
  Vecu16 *set = vecu16_alloc(genSizeUpper);
  Vecu16 *res = vecu16_alloc(n);
  Vecu16 *util = vecu16_alloc(n);
  Vecu16 *orderVec = group_getOrderVector_alloc(parent);
  rand_setSeed(41);
  for(i32 i = 0; i < iter; i++) {
    u32 numGen = rand_getU32InRange(genSizeLower, genSizeUpper);
    vecu16_fill(set, 0xffff);
    for(i32 j = 0; j < numGen; j++) {
      u16 randEle = 0xffff;
      bool validEle = 0;
      while(!validEle) {
        randEle = rand_getU32InRange(1, n - 1);
        validEle = !vecu16_contains(set, randEle, 0);
        if(!validEle) {
          continue;
        }
        // Check prime factor condition
        u16 l = group_elementOrder(parent, randEle);
        validEle = order % l == 0;
      }
      *vecu16_at(set, j) = randEle;
    }
    //printf("Generating with set ...\n");
    //vecu16_print(set);
    bool ok = group_generateFromConstr_noalloc(parent, set, res, util,
                                               orderVec, order);
    if(ok) {
      group_truncGeneratedSet(res, 0);
      u32 m = 0;
      vecu16_indexOf(res, 0xffff, &m, 0);
      if(m == order) {
        printf("%05u :: %u :: Found subgrp of order %u.\n", i + 1, numGen, m);
      }
    } else {

    }
  }
  vecu16_free(orderVec);
  vecu16_free(util);
  vecu16_free(res);
  vecu16_free(set);
  return 0;
}

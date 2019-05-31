#include "group_library.h"

#include "../group/group_io.h"

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <dirent.h>


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

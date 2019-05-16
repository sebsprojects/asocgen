#ifndef GROUP_IO
#define GROUP_IO

#include "group.h"


struct GroupMetaInfo {
  char *name;
  u64 djb2Hash;
  u16 order;
  bool isCommutative;
  Vecu16 *minGenSet;
};
typedef struct GroupMetaInfo GroupMetaInfo;

/*
 * Tries to write the group to a file according to the following procedure:
 *   > Determine the maximal space a base-16 encoded group element needs
 *   > Write all of group->gtab encoded to base-16
 *     sequentially into a string where every group element is space-padded
 *     to fill the maximal space.
 *   > Do this process again but without any padding and calculate
 *     hash_djb2Reverse on this new string to serve as part of the filename
 *   > Construct the filename by <group_order>_<hash>.asoc.txt where
 *     group_order is base-10 encoded and 0-padded to 5 characters
 *     (65534 is the maximal group order due to u16 restriction)
 *     Prepended to the above scheme can be an "u" if the isomorphism class
 *     of the group is not fully determined
 *   > Write a new file to path with this filename containing a number of
 *     metadata-lines starting with '#', a blank line and then the previously
 *     constructed padded gtab string followed by a terminating '\n'
 *   > Metadata may include (subject to change):
 *       - Asocgen version
 *       - Write date
 *       - Group isCommutative
 *       - Group name (if available)
 *
 * If a file with the constructed file name already exists or there occurs
 * a filesystem related error, 0 is returned
 * If the file was written successfully, 1 is returned
 */
bool group_writeToFile(Group *group, char *path);
bool group_writeIndexedToFile(Group *group, char *path);

i32 group_sprintGTab(char *buf, Group *group);
i32 group_sprintFileName(char *buf, Group* group, u64 hash);
i32 group_sprintHeader(char *buf, GroupMetaInfo meta);

i32 group_sprintPreamble(char *buf);
i32 group_sprintFileMeta(char *buf);
i32 group_sprintGroupMeta(char *buf, GroupMetaInfo meta);

Group* group_readFromFile_alloc(char *path);


#endif

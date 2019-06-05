#ifndef GROUP_LIBRARY
#define GROUP_LIBRARY

#include "../group/group.h"

#include <elfc_common.h>
#include <elfc_vecptr.h>


// --------------------------------------------------------------------------
// Writing constructable groups
// --------------------------------------------------------------------------

bool app_writeGroup(char *path, Group *group, char *name);

bool app_writeGroupSn(char *path, u32 n);
bool app_writeGroupCn(char *path, u32 n);


// --------------------------------------------------------------------------
// Read group files
// --------------------------------------------------------------------------

/*
 * Get a list of all group file names in directory path. Allocates vecptr and
 * the contained strings
 * If order > 0 and/or isCommutative == 0 or 1 these restrictions will be
 * applied to file names found
 * If order == 0 and/or isCommutative < 1 no such restrictions are applied
 */
Vecptr *app_listGroupFiles_alloc(char *path, u16 order, i32 isCommutative);


// --------------------------------------------------------------------------
// Search for groups
// --------------------------------------------------------------------------

Group *app_searchForGroup_alloc(Group *parent,
                                u16 order,
                                u32 genSizeLower,
                                u32 genSizeUpper,
                                u32 iter);

Vecptr *app_searchForGroups_alloc(Group *parent,
                                  u16 order,
                                  u32 genSizeLower,
                                  u32 genSizeUpper,
                                  u32 iter);

#endif

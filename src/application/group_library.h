#ifndef GROUP_LIBRARY
#define GROUP_LIBRARY

#include <elfc_common.h>
#include <elfc_vecptr.h>


/*
 * Get a list of all group file names in directory path. Allocates vecptr and
 * the contained strings
 * If order > 0 and/or isCommutative == 0 or 1 these restrictions will be
 * applied to file names found
 * If order == 0 and/or isCommutative < 1 no such restrictions are applied
 */
Vecptr *app_listGroupFiles_alloc(char *path, u16 order, i32 isCommutative);


#endif

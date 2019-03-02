#ifndef GROUP_IO
#define GROUP_IO

#include "group.h"

/*
 * Write the group to a file according to the following procedure:
 *   > Determine the maximal space a base-16 encoded group element needs
 *   > Write all of group->gtab encoded to base-16
 *     sequentially into a string where every group element is space-padded
 *     to fill the maximal space. Break lines such that a line limit of 80
 *     characters (without \n) is not exceeded
 *   > Do this process again but without any linebreaks and calculate
 *     hash_djb2Reverse on this new string to serve as part of the filename
 *   > Construct the filename by <group_order>_<hash>.txt where group_order
 *     is base-10 encoded and 0-padded to 5 characters (65534 is the maximal
 *     group order due to u16 restriction)
 *   > Write a new file with this filename and the previously constructed
 *     string containing the encoded gtab
 */
void group_writeToFile(Group *group);

void group_readFromFile(Group *group);


#endif

#include <string.h>

#include "test_group.c"
//#include "test_group_gen.c"

int main(int argc, char **argv) {
  if(argc < 2 || !strcmp(argv[1], "group")) test_group();
  //if(argc < 2 || !strcmp(argv[1], "group_gen")) test_group();
  return 0;
}

#include <string.h>

#include "test_array.c"
#include "test_map.c"
#include "test_math.c"
#include "test_print.c"
#include "test_smallgroup.c"

int main(int argc, char **argv) {
  if(argc < 2 || !strcmp(argv[1], "array")) test_suite_array();
  if(argc < 2 || !strcmp(argv[1], "map")) test_suite_map();
  if(argc < 2 || !strcmp(argv[1], "math")) test_suite_math();
  if(argc < 2 || !strcmp(argv[1], "smallgroup")) test_suite_smallgroup();
  if(argc < 2 || !strcmp(argv[1], "print")) test_suite_print();
  return 0;
}

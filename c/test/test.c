#include <string.h>

#include "test_array.c"

int main(int argc, char **argv) {
  if(argc < 2 || !strcmp(argv[1], "array")) test_suite_array();
  return 0;
}

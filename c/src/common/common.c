#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "common.h"


void padString(char *string, uint8_t val) {
  if(val < 10) sprintf(string + strlen(string), "  ");
  else if(val < 100) sprintf(string + strlen(string), " ");

}

void sprint_uint8(char *string, uint8_t val) {
  if(val == 0xff) {
    padString(string, 0);
    sprintf(string + strlen(string), "~ ");
  } else {
    padString(string, val);
    sprintf(string + strlen(string), "%u ", val);
  }
}

void sprintArray_uint8(char *string, Array_uint8* arr) {
  uint32_t n = arr->size;
  uint32_t i;
  string[0] = '\0';

  sprintf(string, "[ ");
  for(i = 0; i < n; i++) {
    padString(string, *at_uint8(arr, i));
    sprintf(string + strlen(string), "%u", *at_uint8(arr, i));
    if(i < n - 1) sprintf(string + strlen(string), ", ");
    if((i + 1) % 10 == 0) sprintf(string + strlen(string), "\n  ");
  }
  sprintf(string + strlen(string), " ]\n");
}

void printArray_uint8(char *string, Array_uint8* arr) {
  sprintArray_uint8(string, arr);
  printf(string);
}

void sprintArraySquare_uint8(char *string, Array_uint8* arr, uint32_t sq) {
  uint32_t i, j;
  uint8_t ele;
  string[0] = '\0';

  for(i = 0; i < sq; i++) {
    for(j = 0; j < sq; j++) {
      ele = *at_uint8(arr, i * sq + j);
      sprint_uint8(string, ele);
    }
    sprintf(string + strlen(string), "\n");
  }
}

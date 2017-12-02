#include "sarray8.h"

#include <stdarg.h>

#ifdef STRUCT_HACK // -------------------------------------------------------

Array_uint8 *aui8_alloc(uint32_t size) {
  Array_uint8 *array = malloc(sizeof(Array_uint8) + size * sizeof(uint8_t));
  array->size = size;
  array->data = (uint8_t*) array + sizeof(Array_uint8);
  return array;
}

void aui8_free(Array_uint8 *array) {
  free(array);
}

#else // --------------------------------------------------------------------

Array_uint8 *aui8_alloc(uint32_t size) {
  Array_uint8 *array = malloc(sizeof(Array_uint8));
  array->size = size;
  array->data = malloc(size * sizeof(uint8_t));
  return array;
}

void aui8_free(Array_uint8 *array) {
  free(array->data);
  free(array);
}

#endif // -------------------------------------------------------------------


Array_uint8 *aui8_allocN(uint32_t n, ...) {
  Array_uint8 *array = aui8_alloc(n);
  va_list args;
  va_start(args, n);
  uint32_t i;
  for(i = 0; i < n; i++) {
    array->data[i] = va_arg(args, int);
  }
  va_end(args);
  return array;
}

// --------------------------------------------------------------------------

void aui8_sprint(char *string, Array_uint8 *arr) {
  uint32_t n = arr->size;
  uint32_t i;
  string[0] = '\0';
  sprintf(string, "[ ");
  for(i = 0; i < n; i++) {
    padStringForInt(string, *aui8_at(arr, i));
    sprintf(string + strlen(string), "%u", *aui8_at(arr, i));
    if(i < n - 1) sprintf(string + strlen(string), ", ");
    if((i + 1) % 10 == 0) sprintf(string + strlen(string), "\n  ");
  }
  sprintf(string + strlen(string), " ]\n");
}

void aui8_sprintSquare(char *string, Array_uint8 *arr, uint32_t sq) {
  uint32_t i, j;
  uint8_t ele;
  string[0] = '\0';
  for(i = 0; i < sq; i++) {
    for(j = 0; j < sq; j++) {
      ele = *aui8_at(arr, i * sq + j);
      sprint_uint(string, ele);
    }
    sprintf(string + strlen(string), "\n");
  }
}

void aui8_print(char *string, Array_uint8 *arr) {
  aui8_sprint(string, arr);
  printf(string);
}

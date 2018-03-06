#include "sarray8.h"

#include "sarray16.h"
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

void aui8_sprintToNum(Array_uint8 *arr, char *pstring,
                      uint32_t elePerLine, uint32_t indent) {
  Array_uint16 *aui16 = aui16_allocFromAui8(arr);
  aui16_sprintToNum(aui16, pstring, elePerLine, indent);
  aui16_free(aui16);

}

void aui8_sprintToWidth(Array_uint8 *arr, char *pstring,
                        uint32_t width, uint32_t indent) {
  Array_uint16 *aui16 = aui16_allocFromAui8(arr);
  aui16_sprintToWidth(aui16, pstring, width, indent);
  aui16_free(aui16);
}

void aui8_sprintDefault(Array_uint8 *arr, char *pstring) {
  Array_uint16 *aui16 = aui16_allocFromAui8(arr);
  aui16_sprintDefault(aui16, pstring);
  aui16_free(aui16);
}

void aui8_sprintSquare(Array_uint8 *arr, char *pstring, uint32_t indent) {
  Array_uint16 *aui16 = aui16_allocFromAui8(arr);
  aui16_sprintSquare(aui16, pstring, indent);
  aui16_free(aui16);
}

void aui8_printToNum(Array_uint8 *arr, uint32_t elePerLine, uint32_t indent) {
  Array_uint16 *aui16 = aui16_allocFromAui8(arr);
  aui16_printToNum(aui16, elePerLine, indent);
  aui16_free(aui16);
}

void aui8_printToWidth(Array_uint8 *arr, uint32_t width, uint32_t indent) {
  Array_uint16 *aui16 = aui16_allocFromAui8(arr);
  aui16_printToWidth(aui16, width, indent);
  aui16_free(aui16);
}

void aui8_printDefault(Array_uint8 *arr) {
  Array_uint16 *aui16 = aui16_allocFromAui8(arr);
  aui16_printDefault(aui16);
  aui16_free(aui16);
}

void aui8_printSquare(Array_uint8 *arr, uint32_t indent) {
  Array_uint16 *aui16 = aui16_allocFromAui8(arr);
  aui16_printSquare(aui16, indent);
  aui16_free(aui16);
}

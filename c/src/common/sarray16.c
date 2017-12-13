#include "sarray16.h"

#include "sarray8.h"
#include <stdarg.h>
#include <math.h>

#ifdef STRUCT_HACK // -------------------------------------------------------

Array_uint16 *aui16_alloc(uint32_t size) {
  Array_uint16 *array = malloc(sizeof(Array_uint16) + size *sizeof(uint16_t));
#ifdef BOUNDS_CHECK
  array->_allocSize = size;
#endif
  array->size = size;
  array->data = (uint16_t*) array + sizeof(Array_uint16);
  return array;
}

void aui16_free(Array_uint16 *array) {
  free(array);
}

#else // --------------------------------------------------------------------

Array_uint16 *aui16_alloc(uint32_t size) {
  Array_uint16 *array = malloc(sizeof(Array_uint16));
#ifdef BOUNDS_CHECK
  array->_allocSize = size;
#endif
  array->size = size;
  array->data = malloc(size * sizeof(uint16_t));
  return array;
}

void aui16_free(Array_uint16 *array) {
  free(array->data);
  free(array);
}

#endif // -------------------------------------------------------------------

Array_uint16 *aui16_allocN(uint32_t n, ...) {
  Array_uint16 *array = aui16_alloc(n);
  va_list args;
  va_start(args, n);
  uint32_t i;
  for(i = 0; i < n; i++) {
    array->data[i] = va_arg(args, int);
  }
  va_end(args);
  return array;
}

Array_uint16 *aui16_alloc1(uint16_t e1) {
  return aui16_allocN(1, e1);
}
Array_uint16 *aui16_alloc2(uint16_t e1, uint16_t e2) {
  return aui16_allocN(2, e1, e2);
}
Array_uint16 *aui16_alloc3(uint16_t e1, uint16_t e2, uint16_t e3) {
  return aui16_allocN(3, e1, e2, e3);
}
Array_uint16 *aui16_alloc4(uint16_t e1, uint16_t e2,
                           uint16_t e3, uint16_t e4) {
  return aui16_allocN(4, e1, e2, e3, e4);
}
Array_uint16 *aui16_alloc5(uint16_t e1, uint16_t e2, uint16_t e3,
                           uint16_t e4, uint16_t e5) {
  return aui16_allocN(5, e1, e2, e3, e4, e5);
}

Array_uint16 *aui16_allocFromAui8(Array_uint8 *aui8) {
  Array_uint16 *array = aui16_alloc(aui8->size);
  uint32_t i;
  for(i = 0; i < array->size; i++) {
    *aui16_at(array, i) = *aui8_at(aui8, i);
  }
  return array;
}

// -------------------------------------------------------------------------

void aui16_grow(Array_uint16 *array, uint32_t newSize) {
#ifdef BOUNDS_CHECK
  if(array->size > array->_allocSize) {
    fprintf(stderr, "error: Array_uint16 grow: allocsize=%u, newsize=%u \n",
	    array->_allocSize, newSize);
    exit(1);
  }
#endif
  array->size = newSize;
}

void aui16_shrink(Array_uint16 *array, uint32_t newSize) {
#ifdef BOUNDS_CHECK
  if(array->size < newSize) {
    fprintf(stderr, "error: Array_uint16 shrink: size=%u, newsize=%u \n",
	    array->size, newSize);
    exit(1);
  }
#endif
  array->size = newSize;
}

Array_uint16 *aui16_copy(Array_uint16 *array) {
  Array_uint16 *copy = aui16_alloc(array->size);
  uint32_t i;
  for(i = 0; i < array->size; i++) {
    *aui16_at(copy, i) = *aui16_at(array, i);
  }
  return copy;
}

void aui16_copyInto(Array_uint16 *to, Array_uint16* from) {
#ifdef BOUNDS_CHECK
  if(to->size < from->size) {
    fprintf(stderr,
            "error: Array_uint16 copyIntoArray: from->size=%u, to->size=%u",
            from->size, to->size);
    exit(1);
  }
#endif
  uint32_t i;
  for(i = 0; i < from->size; i++) {
    *aui16_at(to, i) = *aui16_at(from, i);
  }
}


void aui16_sort(Array_uint16 *a, uint32_t start, uint32_t end) {
#ifdef BOUNDS_CHECK
  if(end >= a->size || start >= a->size) {
    fprintf(stderr, "error: Array_uint16 sort: size=%u, start=%u, end=%u \n",
	    a->size, start, end);
    exit(1);
  }
#endif
  uint16_t *base = &a->data[start];
  uint32_t num = end - start + 1;
  uint32_t size = sizeof(uint16_t);
  int32_t lessThanCompare(const void *x, const void *y);
  qsort(base, num, size, lessThanCompare);
}

bool aui16_areEqualArrays(Array_uint16 *a, Array_uint16 *b) {
  if(a->size != b->size) {
    return 0;
  }
  uint32_t i;
  for(i = 0; i < a->size; i++) {
    if(*aui16_at(a, i) != *aui16_at(b, i)) {
      return 0;
    }
  }
  return 1;
}

bool aui16_haveEqualContent(Array_uint16 *a, Array_uint16 *b) {
  fprintf(stderr, "error: aui16_haveEqualContent is not implemented yet\n");
  exit(1);
}

bool aui16_hasDuplicates(Array_uint16 *array) {
  uint32_t i, j;
  uint16_t ele;
  for(i = 0; i < array->size; i++) {
    ele = *aui16_at(array, i);
    for(j = 0; j < i; j++) {
      if(j == i) {
        continue;
      }
      if(ele == *aui16_at(array, j)) {
        return 1;
      }
    }
  }
  return 0;
}

uint16_t aui16_getMax(Array_uint16 *array) {
  uint32_t i;
  uint16_t ele;
  uint16_t max = 0;
  for(i = 0; i < array->size; i++) {
    ele = *aui16_at(array, i);
    if(ele > max) {
      max = ele;
    }
  }
  return max;
}

uint16_t aui16_getMin(Array_uint16 *array) {
  uint32_t i;
  uint16_t ele;
  uint16_t min = 0xffff;
  for(i = 0; i < array->size; i++) {
    if(ele < min) {
      min = ele;
    }
  }
  return min;
}

// -------------------------------------------------------------------------

bool aui16_isProperSet(Array_uint16 *array) {
  return !aui16_hasDuplicates(array);
}

bool aui16_areEqualSets(Array_uint16 *a, Array_uint16 *b) {
  return aui16_isSubset(a, b) && aui16_isSubset(b, a);
}

bool aui16_isSubset(Array_uint16 *sub, Array_uint16 *set) {
  uint32_t i;
  for(i = 0; i < sub->size; i++) {
    if(aui16_indexOf(set, *aui16_at(sub, i)) >= set->size) {
      return 0;
    }
  }
  return 1;
}

// -------------------------------------------------------------------------

void aui16_sprintLine(Array_uint16 *arr, char *pstring,
                      uint32_t indFrom, uint32_t indTo, uint32_t indent,
                      uint32_t digitPad) {
  uint32_t i;
  uint16_t ele;
  padString(pstring, indent);
  for(i = indFrom; i < indTo; i++) {
    ele = *aui16_at(arr, i);
    padStringForInt(pstring, ele, digitPad);
    sprintf(pstring + strlen(pstring), "%u", ele);
    if(i < indTo - 1) {
      sprintf(pstring + strlen(pstring), ", ");
    }
  }
}

void aui16_sprintToNumWithPad(Array_uint16 *arr, char *pstring,
                               uint32_t elePerLine, uint32_t indent,
                               uint32_t digitPad) {
  if(arr->size == 0) {
    sprintf(pstring + strlen(pstring), "[   ]");
    return;
  }
  uint32_t n = arr->size;
  uint32_t numLines = ceil((float) n / elePerLine);
  uint32_t eleInLastLine = n % elePerLine;
  uint32_t entrySize = 2 + digitPad;
  // first line
  padString(pstring + strlen(pstring), indent); // manual indent b/c "["
  sprintf(pstring + strlen(pstring), "[ ");
  aui16_sprintLine(arr, pstring, 0, umin(arr->size, elePerLine), 0, digitPad);
  uint32_t i;
  // 1 until before last line
  for(i = 1; i < numLines - 1; i++) {
    sprintf(pstring + strlen(pstring), ",\n  ");
    aui16_sprintLine(arr, pstring, i * elePerLine, (i + 1) * elePerLine,
                     indent, digitPad);
  }
  // last line
  if(numLines > 1) {
    sprintf(pstring + strlen(pstring), ",\n  ");
    aui16_sprintLine(arr, pstring, (numLines - 1) * elePerLine,
                     umin(arr->size, numLines * elePerLine),
                     indent, digitPad);
    if(eleInLastLine > 0) {
      // pads until the end of the line
      padString(pstring, (elePerLine - eleInLastLine) * entrySize);
    }
  }
  sprintf(pstring + strlen(pstring), " ]");
}

void aui16_sprintToNum(Array_uint16 *arr, char *pstring,
                       uint32_t elePerLine, uint32_t indent) {
  uint16_t max = aui16_getMax(arr);
  uint32_t digitPad = max == 0 ? 1 : floor(log10(max)) + 1;
  aui16_sprintToNumWithPad(arr, pstring, elePerLine, indent, digitPad);
}

void aui16_sprintToWidth(Array_uint16 *arr, char *pstring, uint32_t width,
                       uint32_t indent) {
  uint16_t max = aui16_getMax(arr);
  uint32_t digitPad = max == 0 ? 1 : floor(log10(max)) + 1;
  uint32_t entrySize = 2 + digitPad;
  uint32_t additionalSpace = 2 + 2;
  if(width < indent + additionalSpace + entrySize) { // not enough line wid
    return;
  }
  uint32_t elePerLine = (width - indent - additionalSpace) / entrySize;
  aui16_sprintToNumWithPad(arr, pstring, elePerLine, indent, digitPad);
}

void aui16_sprintDefault(Array_uint16 *arr, char *pstring) {
  aui16_sprintToWidth(arr, pstring, 80, 0);
}

void aui16_sprintSquare(Array_uint16 *arr, char *pstring, uint32_t indent) {
  uint32_t sq = ceil(sqrt(arr->size));
  aui16_sprintToNum(arr, pstring, sq, indent);
}

void aui16_printToNum(Array_uint16 *arr,uint32_t elePerLine,uint32_t indent) {
  char *pstring = malloc(128 + arr->size * 16); // pretty safe guess I think
  pstring[0] = '\0';
  aui16_sprintToNum(arr, pstring, elePerLine, indent);
  printf("%s\n", pstring);
  free(pstring);
}

void aui16_printToWidth(Array_uint16 *arr, uint32_t width, uint32_t indent) {
  char *pstring = malloc(128 + arr->size * 16); // pretty safe guess I think
  pstring[0] = '\0';
  aui16_sprintToWidth(arr, pstring, width, indent);
  printf("%s\n", pstring);
  free(pstring);
}

void aui16_printDefault(Array_uint16 *arr) {
  char *pstring = malloc(128 + arr->size * 16); // pretty safe guess I think
  pstring[0] = '\0';
  aui16_sprintDefault(arr, pstring);
  printf("%s\n", pstring);
  free(pstring);
}

void aui16_printSquare(Array_uint16 *arr, uint32_t indent) {
  char *pstring = malloc(128 + arr->size * 16); // pretty safe guess I think
  pstring[0] = '\0';
  aui16_sprintSquare(arr, pstring, indent);
  printf("%s\n", pstring);
  free(pstring);
}

// -------------------------------------------------------------------------

int32_t lessThanCompare(const void *x, const void *y) {
  uint16_t xx = *(uint16_t*) x;
  uint16_t yy = *(uint16_t*) y;
  if(xx < yy) {
    return -1;
  }
  if(xx > yy) {
    return 1;
  }
  return 0;
}

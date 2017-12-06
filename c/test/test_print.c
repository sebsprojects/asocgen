#include "test.h"

void test_pad() {
  printTestHead("print", "pad");
  bool ok = 1;
  char s[100]; // guess
  s[0] = '\0';
  padStringForInt(s, 0, 4); sprintf(s + strlen(s), "%u ", 0);
  padStringForInt(s, 9, 4); sprintf(s + strlen(s), "%u ", 9);
  padStringForInt(s, 10, 4); sprintf(s + strlen(s), "%u ", 10);
  padStringForInt(s, 99, 4); sprintf(s + strlen(s), "%u ", 99);
  sprintf(s + strlen(s), "\n");
  padStringForInt(s, 100, 4); sprintf(s + strlen(s), "%u ", 100);
  padStringForInt(s, 999, 4); sprintf(s + strlen(s), "%u ", 999);
  padStringForInt(s, 1000, 4); sprintf(s + strlen(s), "%u ", 1000);
  padStringForInt(s, 9999, 4); sprintf(s + strlen(s), "%u ", 9999);
  printf("%s\n", s);
  printTestFoot(ok);
}

void test_array_empty() {
  printTestHead("print", "empty");
  bool ok = 1;
  Array_uint16 *array = aui16_alloc(0);
  char pstring[2000];
  pstring[0] = '\0';
  aui16_sprintToWidth(array, pstring, 80, 0);
  printf("%s\n", pstring);
  aui16_free(array);
  printTestFoot(ok);

}

void test_array_line() {
  printTestHead("print", "line");
  bool ok = 1;
  Array_uint16 *array = aui16_alloc(40);
  uint32_t i;
  for(i = 0; i < array->size; i++) {
    *aui16_at(array, i) = (1001 * i * i) % 0xfffe;
  }
  char pstring[2000];
  pstring[0] = '\0';
  aui16_sprintLine(array, pstring, 0, 10, 0, 5);
  printf("%s\n", pstring);
  aui16_free(array);
  printTestFoot(ok);
}

void test_array_single() {
  printTestHead("print", "array single");
  bool ok = 1;
  Array_uint16 *array = aui16_alloc3(55, 999, 65000);
  char pstring[2000];
  pstring[0] = '\0';
  aui16_sprintToWidth(array, pstring, 80, 0);
  printf("%s\n", pstring);
  aui16_free(array);
  printTestFoot(ok);

}

void test_array_multi() {
  printTestHead("print", "array multi");
  bool ok = 1;
  Array_uint16 *array = aui16_alloc(11);
  uint32_t i;
  for(i = 0; i < array->size; i++) {
    *aui16_at(array, i) = (1001 * i * i) % 0xfffe;
  }
  char pstring[2000];
  pstring[0] = '\0';
  aui16_sprintToWidth(array, pstring, 80, 0);
  printf("%s\n", pstring);
  aui16_free(array);
  printTestFoot(ok);
}

void test_array_num() {
  printTestHead("print", "array num");
  bool ok = 1;
  Array_uint16 *array = aui16_alloc(11);
  uint32_t i;
  for(i = 0; i < array->size; i++) {
    *aui16_at(array, i) = (1001 * i * i) % 0xfffe;
  }
  char pstring[2000];
  pstring[0] = '\0';
  aui16_sprintToNum(array, pstring, 3, 0);
  printf("%s\n", pstring);
  aui16_free(array);
  printTestFoot(ok);
}

void test_array_print() {
  printTestHead("print", "array print");
  bool ok = 1;
  Array_uint16 *array = aui16_alloc(20);
  uint32_t i;
  for(i = 0; i < array->size; i++) {
    *aui16_at(array, i) = (1001 * i * i) % 0xfffe;
  }
  aui16_printToNum(array, 10, 10);
  aui16_free(array);
  printTestFoot(ok);
}

void test_array_square_proper() {
  printTestHead("print", "array square proper");
  bool ok = 1;
  Array_uint16 *array = aui16_alloc(36);
  uint32_t i;
  for(i = 0; i < array->size; i++) {
    *aui16_at(array, i) = (1001 * i * i) % 0xfffe;
  }
  aui16_printSquare(array, 5);
  aui16_free(array);
  printTestFoot(ok);
}

void test_array_square_improper() {
  printTestHead("print", "array square improper");
  bool ok = 1;
  Array_uint16 *array = aui16_alloc(37);
  uint32_t i;
  for(i = 0; i < array->size; i++) {
    *aui16_at(array, i) = (1001 * i * i) % 0xfffe;
  }
  aui16_printSquare(array, 0);
  aui16_free(array);
  printTestFoot(ok);
}

void test_array_big() {
  printTestHead("print", "array big");
  bool ok = 1;
  Array_uint16 *array = aui16_alloc(2000);
  uint32_t i;
  for(i = 0; i < array->size; i++) {
    *aui16_at(array, i) = (1001 * i * i) % 0xfffe;
  }
  aui16_printToWidth(array, 170, 0);
  aui16_free(array);
  printTestFoot(ok);
}

void test_array_8() {
  printTestHead("print", "8 bit");
  bool ok = 1;
  Array_uint8 *array = aui8_alloc(21);
  uint32_t i;
  for(i = 0; i < array->size; i++) {
    *aui8_at(array, i) = (1001 * i * i) % 0xfe;
  }
  aui8_printToWidth(array, 50, 20);
  aui8_free(array);
  printTestFoot(ok);
}

void test_suite_print() {
  test_pad();
  test_array_empty();
  test_array_line();
  test_array_single();
  test_array_multi();
  test_array_num();
  test_array_print();
  test_array_square_proper();
  test_array_square_improper();
  //test_array_big();
  test_array_8();
}

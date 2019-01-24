#include <elfc_test.h>

#include "../src/group/group.h"
#include "../src/group/group_common.h"


bool test_c102()
{
  bool ok = 1;
  Group *c102 = createCn(102);
  ok = group_isValid(c102);
  group_free(c102);
  return ok;
}

void test_group()
{
  test_printHeader("group");
  test_printMessage(test_c102(), "c102");
  test_printFooter();
}

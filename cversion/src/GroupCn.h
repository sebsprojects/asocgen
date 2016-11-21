#ifndef GROUPCN
#define GROUPCN

#include "Group.h"


class GroupCn : public Group<int> {

public:
  GroupCn(int n);
  ~GroupCn();

  virtual int op(int a, int b) const;

};

#endif

#ifndef __BUILDER_H_
#define __BUILDER_H_

#include "dddcpp.h"

#include <vector>

extern real zero;
extern real *vars;

void my_init(int nvars);
ddd create_le(int x, int cst);
ddd create_ge(int x, int cst);
ddd create_diff(int x, int y, int cst);
ddd create_eq(int x, int cst);
ddd create_eq2(int x, int y);
ddd create_tuple(const std::vector<int> &sig, const std::vector<int> &tup);
ddd create_rel(const std::vector<int> &sig, const std::vector<std::vector<int> > &data);

#endif

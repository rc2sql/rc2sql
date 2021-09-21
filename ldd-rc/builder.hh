#ifndef __BUILDER_H_
#define __BUILDER_H_

#include "tvpiInt.h"

#include <vector>

extern const int vars;

LddNode *create_bot(LddManager *ldd);
LddNode *create_top(LddManager *ldd);
LddNode *create_not(LddNode *arg);
LddNode *create_and(LddManager *ldd, LddNode *arg1, LddNode *arg2);
LddNode *create_or(LddManager *ldd, LddNode *arg1, LddNode *arg2);
LddNode *create_ex(LddManager *ldd, LddNode *arg, int x);
LddNode *create_all(LddManager *ldd, LddNode *arg, int x);
LddNode *create_le(LddManager *ldd, theory_t *t, int x, int cst);
LddNode *create_ge(LddManager *ldd, theory_t *t, int x, int cst);
LddNode *create_diff(LddManager *ldd, theory_t *t, int x, int y, int cst);
LddNode *create_eq(LddManager *ldd, theory_t *t, int x, int cst);
LddNode *create_eq2(LddManager *ldd, theory_t *t, int x, int y);
LddNode *create_tuple(LddManager *ldd, theory_t *t, const std::vector<int> &sig, const std::vector<int> &data);
LddNode *create_rel(LddManager *ldd, theory_t *t, const std::vector<int> &sig, const std::vector<std::vector<int> > &data);

#endif

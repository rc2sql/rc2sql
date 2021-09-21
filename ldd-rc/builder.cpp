#include "util.h"

#include "builder.hh"

const int vars = 50;

LddNode *create_bot(LddManager *ldd)
{
  LddNode *res = Ldd_GetFalse(ldd);
  Ldd_Ref(res);

  return res;
}

LddNode *create_top(LddManager *ldd)
{
  LddNode *res = Ldd_GetTrue(ldd);
  Ldd_Ref(res);

  return res;
}

LddNode *create_not(LddNode *arg)
{
  LddNode *res = Ldd_Not(arg);
  Ldd_Ref(res);

  return res;
}

LddNode *create_and(LddManager *ldd, LddNode *arg1, LddNode *arg2)
{
  LddNode *res = Ldd_And(ldd, arg1, arg2);
  Ldd_Ref(res);

  return res;
}

LddNode *create_or(LddManager *ldd, LddNode *arg1, LddNode *arg2)
{
  LddNode *res = Ldd_Or(ldd, arg1, arg2);
  Ldd_Ref(res);

  return res;
}

LddNode *create_ex(LddManager *ldd, LddNode *arg, int x)
{
  LddNode *res = Ldd_ExistsAbstract(ldd, arg, x);
  Ldd_Ref(res);

  return res;
}

LddNode *create_all(LddManager *ldd, LddNode *arg, int x)
{
  LddNode *res = Ldd_UnivAbstract(ldd, arg, x);
  Ldd_Ref(res);

  return res;
}

LddNode *create_le(LddManager *ldd, theory_t *t, int x, int cst)
{
  int cf[vars] = {0};
  cf[x] = 1;
  linterm_t trm = t->create_linterm(cf, vars);
  constant_t c = t->create_int_cst(cst);
  lincons_t cons = t->create_cons(trm, 0, c);
  LddNode *ret = Ldd_FromCons(ldd, cons);
  Ldd_Ref(ret);

  return ret;
}

LddNode *create_ge(LddManager *ldd, theory_t *t, int x, int cst)
{
  int cf[vars] = {0};
  cf[x] = -1;
  linterm_t trm = t->create_linterm(cf, vars);
  constant_t c = t->create_int_cst(-cst);
  lincons_t cons = t->create_cons(trm, 0, c);
  LddNode *ret = Ldd_FromCons(ldd, cons);
  Ldd_Ref(ret);

  return ret;
}

LddNode *create_diff(LddManager *ldd, theory_t *t, int x, int y, int cst) {
  int cf[vars] = {0};
  cf[x] = 1;
  cf[y] = -1;
  linterm_t trm = t->create_linterm(cf, vars);
  constant_t c = t->create_int_cst(cst);
  lincons_t cons = t->create_cons(trm, 0, c);
  LddNode *res = Ldd_FromCons(ldd, cons);
  Ldd_Ref(res);

  return res;
}

LddNode *create_eq(LddManager *ldd, theory_t *t, int x, int cst)
{
  LddNode *dp = create_le(ldd, t, x, cst);
  LddNode *dm = create_ge(ldd, t, x, cst);
  return create_and(ldd, dp, dm);
}

LddNode *create_eq2(LddManager *ldd, theory_t *t, int x, int y)
{
  LddNode *dp = create_diff(ldd, t, x, y, 0);
  LddNode *dm = create_diff(ldd, t, y, x, 0);
  return create_and(ldd, dp, dm);
}

LddNode *create_tuple(LddManager *ldd, theory_t *t, const std::vector<int> &sig, const std::vector<int> &data)
{
  LddNode *res = create_top(ldd);
  assert(sig.size() == data.size());
  for (unsigned i = 0; i < data.size(); i++) {
    LddNode *eq = create_eq(ldd, t, sig[i], data[i]);
    res = create_and(ldd, res, eq);
  }
  return res;
}

LddNode *create_rel(LddManager *ldd, theory_t *t, const std::vector<int> &sig, const std::vector<std::vector<int> > &data)
{
  LddNode *res = create_bot(ldd);
  for (unsigned i = 0; i < data.size(); i++) {
    LddNode *tuple = create_tuple(ldd, t, sig, data[i]);
    res = create_or(ldd, res, tuple);
  }

  return res;
}

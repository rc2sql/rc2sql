#include "builder.hh"

real zero;
real *vars;

void my_init(int nvars)
{
  zero = real("zero");
  vars = new real[nvars];
  for (int i = 0; i < nvars; i++) {
    static char tmp[100];
    sprintf(tmp, "x%d", i);
    vars[i] = real(tmp);
  }
}

ddd create_le(int x, int cst)
{
  return vars[x] - zero <= cst;
}

ddd create_ge(int x, int cst)
{
  return zero - vars[x] <= -cst;
}

ddd create_diff(int x, int y, int cst)
{
  return vars[x] - vars[y] <= cst;
}

ddd create_eq(int x, int cst)
{
  ddd dp = create_le(x, cst);
  ddd dm = create_ge(x, cst);
  return dp & dm;
}

ddd create_eq2(int x, int y)
{
  ddd dp = create_diff(x, y, 0);
  ddd dm = create_diff(y, x, 0);
  return dp & dm;
}

ddd create_tuple(const std::vector<int> &sig, const std::vector<int> &tup)
{
  ddd res = True;
  assert(sig.size() == tup.size());
  for (unsigned i = 0; i < tup.size(); i++) {
    ddd eq = create_eq(sig[i], tup[i]);
    res = res & eq;
  }
  return res;
}

ddd create_rel(const std::vector<int> &sig, const std::vector<std::vector<int> > &data)
{
  ddd rel = False;
  for (unsigned i = 0; i < data.size(); i++) {
    ddd tuple = create_tuple(sig, data[i]);
    rel = rel | tuple;
  }

  return rel;

}

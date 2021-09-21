#include "builder.hh"
#include "tvpi.h"

#include "formula.hh"
#include "driver.hh"

#include "db.hh"

#include "visitors.h"

#include <cstdio>
#include <string>

Formula *getAST(const char *fname)
{
  driver drv;
  int res = drv.parse(fname);
  if (res) return NULL;

  return drv.res;
}

int main(int argc, char **argv)
{
  theory_t *t;
  DdManager *cudd;
  LddManager *ldd;

  t = tvpi_create_utvpiz_theory(vars);
  cudd = Cudd_Init(0, 0, CUDD_UNIQUE_SLOTS, CUDD_CACHE_SLOTS, 0);
  ldd = Ldd_Init(cudd, t);

  Formula *fmla = getAST(argv[1]);

  FILE *db_file = fopen(argv[2], "r");
  char *db_buf = NULL;
  size_t db_buf_len = 0;
  size_t db_len = getline(&db_buf, &db_buf_len, db_file);
  fclose(db_file);

  std::map<std::string, std::vector<std::vector<int> > > db = parse_db(db_buf, db_len);

  EvalVisitor eval(ldd, t, db);
  fmla->accept(eval);
  LddNode *res = eval.get();
  eval.print_vars();
  Ldd_PrintMinterm(ldd, res);

  Ldd_Quit(ldd);
  Cudd_Quit(cudd);
  tvpi_destroy_theory(t);

  delete fmla;
  free(db_buf);

  return 0;
}

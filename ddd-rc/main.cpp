#include "formula.hh"
#include "driver.hh"

#include "db.hh"

#include "visitors.h"

#include <cstdio>

Formula *getAST(const char *fname)
{
  driver drv;
  int res = drv.parse(fname);
  if (res) return NULL;

  return drv.res;
}

int main(int argc, char **argv)
{
  ddd::Init();
  my_init(50);

  Formula *fmla = getAST(argv[1]);

  FILE *db_file = fopen(argv[2], "r");
  char *db_buf = NULL;
  size_t db_buf_len = 0;
  size_t db_len = getline(&db_buf, &db_buf_len, db_file);
  fclose(db_file);

  std::map<std::string, std::vector<std::vector<int> > > db = parse_db(db_buf, db_len);

  EvalVisitor eval(db);
  fmla->accept(eval);
  ddd res = eval.get();
  eval.print_vars();
  PrintDNF(res, zero);

  free(db_buf);

  return 0;
}

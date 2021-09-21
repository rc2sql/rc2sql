#ifndef __VISITORS_H__
#define __VISITORS_H__

struct UnifyTermVisitor : public TermVisitor {
  int x;
  int status;
  std::string name;

  UnifyTermVisitor(int x) : x(x) {}

  void visit(VarTerm *trm) {
    status = 2;
    name = trm->name;
  }
  void visit(CstTerm *trm) {
    status = (x == trm->value);
  }
};

class EvalVisitor : public FormulaVisitor {
  LddManager *ldd;
  theory_t *t;
  std::map<std::string, std::vector<std::vector<int> > > &db;
  std::map<std::string, int> var_id;
  int fresh = 0;

  LddNode *res;

  int get_var_id(const std::string &x) {
    auto id = var_id.find(x);
    if (id == var_id.end()) {
      return var_id[x] = fresh++;
    } else {
      return id->second;
    }
  }

public:
  EvalVisitor(LddManager *ldd, theory_t *t, std::map<std::string, std::vector<std::vector<int> > > &db) : ldd(ldd), t(t), db(db) {}
  void print_vars() {
    const char *sep = "";
    for (auto it : var_id) {
      printf("%s%s => x%d", sep, it.first.c_str(), it.second);
      sep = "; ";
    }
    printf("\n");
  }
  LddNode *get() {
    return res;
  }
  void visit(BoolFormula *f) {
    if (f->b) res = create_top(ldd);
    else res = create_bot(ldd);
  }
  void visit(EqFormula *f) {
    LddNode *xy = create_diff(ldd, t, get_var_id(f->x), get_var_id(f->y), 0);
    LddNode *yx = create_diff(ldd, t, get_var_id(f->y), get_var_id(f->x), 0);
    res = create_and(ldd, xy, yx);
  }
  void visit(PredFormula *f) {
    std::vector<int> sig;
    std::set<int> fv;
    for (int i = 0; i < f->trms.size(); i++) {
      UnifyTermVisitor vis(0);
      f->trms[i]->accept(vis);
      if (vis.status == 2) {
        int idx = get_var_id(vis.name);
        if (fv.find(idx) == fv.end()) {
          sig.push_back(idx);
          fv.insert(idx);
        }
      }
    }
    std::vector<std::vector<int> > data;
    for (auto &it : db[f->pred]) {
      std::map<int, int> val;
      int ok = 1;
      for (int i = 0; ok && i < it.size(); i++) {
        UnifyTermVisitor vis(it[i]);
        f->trms[i]->accept(vis);
        if (vis.status == 2) {
          int idx = get_var_id(vis.name);
          if (val.find(idx) == val.end()) {
            val[idx] = it[i];
          } else if (val[idx] != it[i]) {
            ok = 0;
          }
        } else if (vis.status == 0) {
          ok = 0;
        }
      }
      if (ok) {
        std::vector<int> tuple;
        for (int i = 0; i < sig.size(); i++) {
          tuple.push_back(val[sig[i]]);
        }
        data.push_back(tuple);
      }
    }
    res = create_rel(ldd, t, sig, data);
  }
  void visit(NegFormula *f) {
    f->f->accept(*this);
    res = create_not(res);
  }
  void visit(AndFormula *f) {
    f->f->accept(*this);
    LddNode *arg1 = res;
    f->g->accept(*this);
    LddNode *arg2 = res;
    res = create_and(ldd, arg1, arg2);
  }
  void visit(OrFormula *f) {
    f->f->accept(*this);
    LddNode *arg1 = res;
    f->g->accept(*this);
    LddNode *arg2 = res;
    res = create_or(ldd, arg1, arg2);
  }
  void visit(ExFormula *f) {
    f->f->accept(*this);
    res = create_ex(ldd, res, get_var_id(f->x));
  }
};

#endif

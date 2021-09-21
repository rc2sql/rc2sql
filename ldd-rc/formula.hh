#ifndef __FORMULA_H__
#define __FORMULA_H__

#include <limits>
#include <string>
#include <vector>

struct Formula;
struct BoolFormula;
struct EqFormula;
struct PredFormula;
struct NegFormula;
struct AndFormula;
struct OrFormula;
struct ExFormula;

struct Term;
struct VarTerm;
struct CstTerm;

class TermVisitor {
public:
  virtual void visit(VarTerm *trm) = 0;
  virtual void visit(CstTerm *trm) = 0;
};

struct Term {
  virtual void accept(TermVisitor &v) {}
};

struct VarTerm : Term {
  std::string name;

  VarTerm(const std::string &name) : name(name) {};
  void accept(TermVisitor &v) {
    v.visit(this);
  }
};

struct CstTerm : Term {
  int value;

  CstTerm(int value) : value(value) {};
  void accept(TermVisitor &v) {
    v.visit(this);
  }
};

class FormulaVisitor {
public:
    virtual void visit(BoolFormula *f) = 0;
    virtual void visit(EqFormula *f) = 0;
    virtual void visit(PredFormula *f) = 0;
    virtual void visit(NegFormula *f) = 0;
    virtual void visit(AndFormula *f) = 0;
    virtual void visit(OrFormula *f) = 0;
    virtual void visit(ExFormula *f) = 0;
};

struct Formula {
    virtual void accept(FormulaVisitor &v) = 0;
};

struct BoolFormula : Formula {
    bool b;

    BoolFormula(bool b) : b(b) {}
    void accept(FormulaVisitor &v) override {
        v.visit(this);
    }
};

struct EqFormula : Formula {
  std::string x, y;

  EqFormula(const std::string &x, const std::string &y) : x(x), y(y) {}
  void accept(FormulaVisitor &v) override {
    v.visit(this);
  }
};

struct PredFormula : Formula {
    std::string pred;
    std::vector<Term *> trms;

    PredFormula(const std::string &pred, const std::vector<Term *> &trms) : pred(pred), trms(trms) {}
    void accept(FormulaVisitor &v) override {
        v.visit(this);
    }
};

struct NegFormula : Formula {
    Formula *f;

    NegFormula(Formula *f) : f(f) {}
    ~NegFormula() {
        if (f != NULL) delete f;
    }
    void accept(FormulaVisitor &v) override {
        v.visit(this);
    }
};

struct AndFormula : Formula {
    Formula *f, *g;

    AndFormula(Formula *f, Formula *g) : f(f), g(g) {}
    ~AndFormula() {
        if (f != NULL) delete f;
        if (g != NULL) delete g;
    }
    void accept(FormulaVisitor &v) override {
        v.visit(this);
    }
};

struct OrFormula : Formula {
    Formula *f, *g;

    OrFormula(Formula *f, Formula *g) : f(f), g(g) {}
    ~OrFormula() {
        if (f != NULL) delete f;
        if (g != NULL) delete g;
    }
    void accept(FormulaVisitor &v) override {
        v.visit(this);
    }
};

struct ExFormula : Formula {
    std::string x;
    Formula *f;

    ExFormula(const std::string &x, Formula *f) : x(x), f(f) {}
    ~ExFormula() {
        if (f != NULL) delete f;
    }
    void accept(FormulaVisitor &v) override {
        v.visit(this);
    }
};

#endif /* __FORMULA_H__ */

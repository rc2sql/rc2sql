#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <math.h>
#include "dddcpp.h"

boolean::boolean(const char* str, ...) {
  char buf[1024];
  va_list arglist;
  va_start(arglist, str);
  vsprintf(buf, str, arglist);
  m_num = mkBool(buf);
  va_end(arglist);
}

real::real(const char* str, ...) {
  char buf[1024];
  va_list arglist;
  va_start(arglist, str);
  vsprintf(buf, str, arglist);
  m_num = mkVar(buf);
  va_end(arglist);
}

enumerator::enumerator(unsigned int numstates, const char* str, ...) {
  unsigned int i;
  char buf[1024];
  va_list arglist;
  va_start(arglist, str);
  vsprintf(buf, str, arglist);
  va_end(arglist);

  m_numstates = numstates;
  m_numbits = (int)ceil(log(m_numstates)/log(2));
  m_data = (boolean*) calloc(m_numbits, sizeof(boolean));
  for (i=0; i<m_numbits; i++) {
    m_data[i] = boolean("%s%d", buf, i);
  }
  m_num = m_data[0].id();
}

enumerator& enumerator::operator=(const enumerator& e) {
  unsigned int i;

  m_numstates = e.m_numstates;
  m_numbits = e.m_numbits;
  m_num = e.m_num;
  m_data = (boolean*) calloc(m_numbits, sizeof(boolean));

  for (i=0; i<m_numbits; i++) {
    m_data[i] = e[i];
  }

  return *this;
}

varlist::varlist() {
  m_var = new var[m_max=2];
  m_num = 0;
}

varlist::~varlist() {
  delete[] m_var;
}

varlist::varlist(const varlist& l) {
  m_var = new var[m_max=l.m_max];
  m_num = l.m_num;
  for (unsigned int i=0; i<m_num; i++) {
    (*this)[i] = l[i];
  }
}

varlist& varlist::operator=(const varlist& l) {
  m_var = new var[m_max=l.m_max];
  m_num = l.m_num;
  for (unsigned int i=0; i<m_num; i++) {
    (*this)[i] = l[i];
  }
  return *this;
}

void varlist::print() const {
  printf("(");
  for (unsigned int i=0; i<m_num; i++) {
    printf("%s%s", m_var[i].name(), i==m_num-1 ? "" : ",");
  }
  printf(")");
}

unsigned int varlist::length() const {
  return m_num;
}

var& varlist::operator[](unsigned int i) const {
  assert(i<m_num);
  return m_var[i];
}

void varlist::resize(unsigned int new_max) {
  var* tmp = new var[m_max=new_max];
  for (unsigned int i=0; i<m_num; i++) 
    tmp[i] = m_var[i];
  delete[] m_var;
  m_var = tmp;
}

varlist& varlist::add(const var& v) {
  if (m_num==m_max)
    resize(m_max*2);
  m_var[m_num++] = v;
  return *this;
}

varlist& varlist::add(const enumerator& e) {
  unsigned int i;
  for (i=0; i<e.length(); i++)
    add(e[i]);
  return *this;
}

ddd Exists(const varlist& l, const ddd& d) {
  ddd result = d;
  unsigned int i;
  for (i=0; i<l.length(); i++)
    result = Exists(l[i], result);
  return result;
}

ddd Replace(const ddd& d, const varlist& v1, const varlist& v2) {
  ddd result = d;
  if (v1.length() == v2.length()) {
    unsigned int i;
    for (i=0; i<v1.length(); i++)
      result = Replace(result, v1[i], v2[i]);
  }
  return result;
}



void ddd::Init(tVar maxVar, double mb_mem_usage,
	       double mem_ratio) {
  ::init(maxVar, mb_mem_usage, mem_ratio);
}

void ddd::SetErrorHandler(tErrorHandler errorHandler) {
  ::set_error_handler(errorHandler);
}

void ddd::SetPreGC(tPreGC preGC) {
  ::set_pre_gc(preGC);
}


void ddd::AppendToOrder(const real& x, const real& y) {
  ::appendRealToOrder(x.id(), y.id());
}

void ddd::AppendToOrder(const boolean& x) {
  ::appendBoolToOrder(x.id());
}

void ddd::AppendToOrder(const enumerator& e) {
  unsigned int i;
  for (i=0; i<e.length(); i++) {
    ddd::AppendToOrder(e[i]);
  }
}

void ddd::PrependToOrder(const real& x, const real& y) {
  ::prependRealToOrder(x.id(), y.id());
}

void ddd::PrependToOrder(const boolean& x) {
  ::prependBoolToOrder(x.id());
}

void ddd::PrependToOrder(const enumerator& e) {
  unsigned int i;
  for (i=0; i<e.length(); i++) {
    ddd::PrependToOrder(e[i]);
  }
}


void ddd::OrderVariables(enum OrderType type) {
  switch (type) {
  case APPEND_INC_LEX:
    ::appendIncrLexToOrder();
    break;
  case PREPEND_INC_LEX:
    ::prependIncrLexToOrder();
    break;
  case APPEND_DEC_LEX:
    ::appendDecrLexToOrder();
    break;
  case PREPEND_DEC_LEX:
    ::prependDecrLexToOrder();
    break;
  }
}

void ddd::ClearOrder(void) {
  ::clearOrder();
}

void ddd::PrintOrder(void) {
  ::printOrder();
}


void ddd::Done() {
  ::done();
}

void ddd::Info() {
  ::info();
}

void SetGraphName(const char* name, ...) {
  char buf[1024];
  va_list arglist;
  va_start(arglist, name);
  vsprintf(buf, name, arglist);
  ::setGraphName(buf);
  va_end(arglist);
}



boolean& enumerator::operator[](unsigned int bit) const {
  assert(bit<m_numbits);
  return m_data[bit];
}


// DDDs

ddd ddd_true(void)    { return ddd(1,42); }
ddd ddd_false(void)   { return ddd(0,42); }

ddd operator!(const ddd& u) {
 return ddd(::not(u.id()));
}

ddd operator>>(const ddd& u, const ddd& v) {
  return ddd(::imp(u.id(), v.id()));
}

ddd operator==(const ddd& u, const ddd& v) {
  return ddd(::biimp(u.id(), v.id()));
}

ddd operator!=(const ddd& u, const ddd& v) {
  return ddd(::xor(u.id(), v.id()));
}

ddd operator&(const ddd& u, const ddd& v) {
  return ddd(::and(u.id(), v.id()));
}

ddd operator|(const ddd& u, const ddd& v) {
  return ddd(::or(u.id(), v.id()));
}

ddd operator-(const ddd& u, const ddd& v) {
  return ddd(::nimp(u.id(), v.id()));
}

ddd ddd::operator&=(const ddd& v) {
  return *this = *this & v;
}

ddd ddd::operator|=(const ddd& v) {
  return *this = *this | v;
}

ddd ddd::operator-=(const ddd& v) {
  return *this = *this - v;
}

ddd operator==(const enumerator& e, unsigned int state) {
  assert(state<e.states());
  ddd result = True;
  for (unsigned int i=0; i<e.length(); i++) {
    result &= ((1<<i) & state) ? e[i] : !e[i];
  }
  return result;
}


ddd Forall(const var& x, const ddd& u) {
  return ddd(::forall(x.id(), u.id()));
}

ddd Exists(const var& x, const ddd& u) {
  return ddd(::exists(x.id(), u.id()));
}

ddd Exists(const enumerator& e, const ddd& d) {
  ddd result = d;
  for (int i=e.length()-1; i>=0; i--) {
    result = Exists(e[i], result);
  }
  return result;
}

ddd Forall(const enumerator& e, const ddd& d) {
  ddd result = d;
  unsigned int i;
  for (i=0; i<e.length(); i++) {
    result = Forall(e[i], result);
  }
  return result;
}


ddd Assign(const ddd& u, const real& x, const real& y, tCstr c) {
  return ddd(::assign(u.id(), x.id(), y.id(), c));
}

ddd Assign(const ddd& u, const boolean& x, unsigned short int b) {
  return ddd(::assignBool(u.id(), x.id(), b));
}

ddd Replace(const ddd& u, const var& x, const var& y) {
  return ddd(::replace(u.id(), x.id(), y.id()));
}

ddd Replace(const ddd& d, const enumerator& x, const enumerator& y) {
  ddd result = d;
  if (x.states() == y.states()) {
    unsigned int i;
    for (i=0; i<x.length(); i++) {
      result = Replace(result, x[i], y[i]);
    }
  }
  return result;
}

ddd PathReduce(const ddd& u) {
  return ddd(::pathReduce(u.id()));
}

ddd Merge(const ddd& u) {
  return ddd(::merge(u.id()));
}

ddd Saturate(const ddd& u) {
  return ddd(::saturate(u.id()));
}

bool Tautology(const ddd& u) {
  return ::tautology(u.id());
}

bool Satisfiable(const ddd& u) {
  return ::satisfiable(u.id());
}

bool Unsatisfiable(const ddd& u) {
  return ::unsatisfiable(u.id());
}

bool Falsifiable(const ddd& u) {
  return ::falsifiable(u.id());
}

bool Equivalent(const ddd& u, const ddd& v) {
  return ::equivalent(u.id(),v.id());
}

bool Consequence(const ddd& u, const ddd& v) {
  return ::consequence(u.id(),v.id());
}

ddd Hull(const ddd& u) {
  return ddd(::hull(u.id()));
}

void Anysat(const ddd& u) {
  ::anysat(u.id());
}

double PathCount(const ddd& u) {
  return ::pathCount(u.id());
}

unsigned int NodeCount(const ddd& u) {
  return ::nodeCount(u.id());
}

unsigned int VarCount(const ddd& u) {
  return ::varCount(u.id());
}

bool IsFreeVar(const var& x, const ddd& u) {
  return ::isFreeVar(x.id(), u.id());
}

void SaveDDD(const char* name, const ddd& u) {
  ::save_to_file(name, u.id());
}

ddd RestoreDDD(const char* name) {
  return ddd(::restore_from_file(name));
}


void SaveGraph(const char* dotname, const ddd& u) {
  ::saveGraph(dotname, u.id());
}

void ViewGraph(const ddd& u) {
  ::viewGraph(u.id());
}

void SavePlot(const char* texname,
		     const real& z, const real& x, const real& y,
		     const ddd& u) {
  ::savePlot(texname, z.id(), x.id(), y.id(), u.id());
}

void ViewPlot(const real& z, const real& x, const real& y, const ddd& u) {
  ::viewPlot(z.id(), x.id(), y.id(), u.id());
}

void PrintDNF(const ddd& u, const real& z) {
  ::printDNF(u.id(), z.id());
}

void PrintINF(const ddd& u) {
  ::printINF(u.id());
}

void PrintINFpretty(const ddd& u) {
  ::printINFpretty(u.id());
}

void SetGraphOptions(bool asDot, bool graphNumbering) {
  ::setGraphOptions(asDot, graphNumbering);
}

void SetPlotWindow(int xMin, int yMin, int xMax, int yMax) {
  ::setPlotWindow(xMin, yMin, xMax, yMax);
}

void SetPlotSize(int width, int height) {
  ::setPlotSize(width, height);
}

void SetPlotAxis(bool showAxis, bool showNumbers,
                 bool showTicks, bool showAxisInFront) {
  ::setPlotAxis(showAxis,showNumbers,showTicks,showAxisInFront);
}

void SetPlotEdges(bool plotEdges) {
  ::setPlotEdges(plotEdges);
}

void SetPlotColours(tColour fillColour, tColour edgeColour) {
  ::setPlotColours(fillColour, edgeColour);
}

void SetPlotSaveOptions(bool saveAsArticle) {
  ::setPlotSaveOptions(saveAsArticle);
}

void SetPlotPsCode(const char* psCode) {
  ::setPlotPsCode(psCode);
}

difference operator-(const real& p, const real& n) {
  if (p.id()==n.id()) {
    printf("Error: the two variables in a difference\n"
	   "constraint must be different.\n");
    exit(1);
  }
  return difference(p,n);
}

// Boolean operators; the priority is (the usual): !, >>, ==, &, |
// Negation
ddd operator!(const boolean& u)                    { return !ddd(u); }
// Implication
ddd operator>>(const boolean& u, const boolean& v) { return ddd(u) >> ddd(v);}
// Biimplication
ddd operator==(const boolean& u, const boolean& v) { return ddd(u) == ddd(v);}
// not-Biimplication (xor)
ddd operator!=(const boolean& u, const boolean& v) { return ddd(u) != ddd(v);}
// Conjunction
ddd operator&(const boolean& u, const boolean& v)  { return ddd(u) & ddd(v);}
// Disjunction
ddd operator|(const boolean& u, const boolean& v)  { return ddd(u) | ddd(v);}

// Constructors for difference constraints such as x-y<c, etc.
ddd operator<(const difference& c, tCstr v)  { return ddd(mkDiffCstr(c.pos(), c.neg(), LE, v)); }
ddd operator<=(const difference& c, tCstr v) { return ddd(mkDiffCstr(c.pos(), c.neg(), LEQ, v));}
ddd operator==(const difference& c, tCstr v) { return ddd(mkDiffCstr(c.pos(), c.neg(), EQ, v)); }
ddd operator!=(const difference& c, tCstr v) { return ddd(mkDiffCstr(c.pos(), c.neg(), NEQ, v));}
ddd operator>(const difference& c, tCstr v)  { return ddd(mkDiffCstr(c.pos(), c.neg(), GR, v)); }
ddd operator>=(const difference& c, tCstr v) { return ddd(mkDiffCstr(c.pos(), c.neg(), GEQ, v));}

// Constructors for difference constraints such as x<y and x<=y
ddd operator<(tCstr v, const difference& c)  { return c>v; }
ddd operator<=(tCstr v, const difference& c) { return c>=v; }
ddd operator==(tCstr v, const difference& c) { return c==v; }
ddd operator!=(tCstr v, const difference& c) { return c!=v; }
ddd operator>(tCstr v, const difference& c)  { return c<v; }
ddd operator>=(tCstr v, const difference& c) { return c<=v; }

ddd operator<(const real& x, const real& y)  { return x-y<0; }
ddd operator<=(const real& x, const real& y) { return x-y<=0; }
ddd operator==(const real& x, const real& y) { return x-y==0; }
ddd operator!=(const real& x, const real& y) { return x-y!=0; }
ddd operator>=(const real& x, const real& y) { return x-y>=0; }
ddd operator>(const real& x, const real& y)  { return x-y>0; }

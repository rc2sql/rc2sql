#ifndef DDDCPP_H
#define DDDCPP_H

/* Disallow the user to include ddd.h before including this file. */
#ifdef DDD_H
#  error The file 'ddd.h' should not be included when using 'dddcpp.h'
#endif

extern "C" {
#include <stdio.h>
#include <stdlib.h>
#include <ddd.h>
#include <assert.h>
}

// Generic variable class
class var {
protected:
  tVar m_num;
private:
  var(tVar v)                          { m_num = v; }
public:
  var()                                { m_num = (tVar)-1; }
  virtual ~var()                       { }
  tVar id() const                      { return m_num; }
  const char* name() const             { return nameOfVar(m_num); } 
  virtual var& operator=(const var& v) { m_num=v.m_num; return *this; }
};


// Real-valued variables
class real : public var {
private:
public:
  real() : var() {}
  real(const char* name, ...);
};


// Boolean variables
class boolean : public var {
public:
  boolean() : var() {}
  boolean(const char* name, ...);
};

class enumerator : public var {
public:
  enumerator() : var() {};
  enumerator(unsigned int numstates, const char* name, ...);
  enumerator& operator=(const enumerator& e);
  ~enumerator() { free(m_data); }
  unsigned int states() const { return m_numstates; }
  unsigned int length() const { return m_numbits; }
  // Return i'th bit
  boolean& operator[](unsigned int bit) const;
private:
  enumerator(const enumerator& e);
  unsigned int m_numbits;
  unsigned int m_numstates;
  boolean* m_data;
};

class varlist {
public:
  varlist();
  ~varlist();
  varlist(const varlist& l);
  varlist& operator=(const varlist& l);
  void print() const;
  unsigned int length() const;
  var& operator[](unsigned int i) const;
  varlist& add(const var& v);
  varlist& add(const enumerator& v);

private:  
  void resize(unsigned int new_max);
  var *m_var;
  unsigned int m_num;
  unsigned int m_max;
};

// Syntactic sugar for difference between two reals
class difference {
public:
  friend difference operator-(const real& p, const real& n);
  tVar pos() const { return m_p; }
  tVar neg() const { return m_n; }
private:
  difference(const real& p, const real& n) { m_p = p.id(); m_n = n.id(); }
  tVar m_p, m_n;
};

// The different ways of ordering variables
enum OrderType {
  APPEND_INC_LEX,
  PREPEND_INC_LEX,
  APPEND_DEC_LEX,
  PREPEND_DEC_LEX
};

// Class implementing difference constraint expressions af DDDs
// ------------------------------------------------------------
class ddd {
private:
  ddd(tDDD r)             { ::incRef(m_r = r); }

public:
  // Initialize the DDD library with the specified parameters
  static void Init(tVar maxVar = defaultMaxVar,
		   double mb_mem_usage = defaultMbMemUsage,
		   double mem_ratio = defaultMemRatio);
  // Finalize the DDD library
  static void Done();

  static void SetErrorHandler(tErrorHandler errorHandler);
  static void SetPreGC(tPreGC preGC);

  // AppendToOrder(x,y) appends the pair (x,y) to the current
  // ordering.  Calling OrderVariables(PREPEND_DEC_LEX) will prepend
  // the remaining, unordered variables, to the the current ordering
  // in decreasing lexicographic order.
  static void AppendToOrder(const real& x, const real& y);
  static void AppendToOrder(const boolean& x);
  static void AppendToOrder(const enumerator& x);
  static void PrependToOrder(const real& x, const real& y);
  static void PrependToOrder(const boolean& x);
  static void PrependToOrder(const enumerator& x);
  static void OrderVariables(enum OrderType type);
  static void ClearOrder(void);
  static void PrintOrder(void);

  // Print various information
  static void Info();

  // Get the unique id associated with a ddd
  tDDD id() const { return m_r; }

  // Constructors, destructor, and assignment:
# define True                  ddd_true()
# define False                 ddd_false()
  
  ddd(void)                    { m_r=0; }
  ddd(const ddd& d)            { ::incRef(m_r=d.m_r); }
  ddd(const boolean& b)        { ::incRef(m_r=mkBoolVar(b.id())); }

  // Destructing a ddd decrements the reference counter by one
  ~ddd()                       { ::decRef(m_r); }
  ddd& operator=(const ddd& d) { ::decRef(m_r); ::incRef(m_r=d.m_r); return *this; }

  // Boolean operators; the priority is (the usual): !, >>, ==, &, |
  // Negation
  friend ddd operator!(const boolean& u)                    { return !ddd(u); }
  // Implication
  friend ddd operator>>(const boolean& u, const boolean& v) { return ddd(u) >> ddd(v);}
  // Biimplication
  friend ddd operator==(const boolean& u, const boolean& v) { return ddd(u) == ddd(v);}
  // not-Biimplication (xor)
  friend ddd operator!=(const boolean& u, const boolean& v) { return ddd(u) != ddd(v);}
  // Conjunction
  friend ddd operator&(const boolean& u, const boolean& v)  { return ddd(u) & ddd(v);}
  // Disjunction
  friend ddd operator|(const boolean& u, const boolean& v)  { return ddd(u) | ddd(v);}

  // Constructors for difference constraints such as x-y<c, etc.
  friend ddd operator<(const difference& c, tCstr v)  { return ddd(mkDiffCstr(c.pos(), c.neg(), LE, v)); }
  friend ddd operator<=(const difference& c, tCstr v) { return ddd(mkDiffCstr(c.pos(), c.neg(), LEQ, v));}
  friend ddd operator==(const difference& c, tCstr v) { return ddd(mkDiffCstr(c.pos(), c.neg(), EQ, v)); }
  friend ddd operator!=(const difference& c, tCstr v) { return ddd(mkDiffCstr(c.pos(), c.neg(), NEQ, v));}
  friend ddd operator>(const difference& c, tCstr v)  { return ddd(mkDiffCstr(c.pos(), c.neg(), GR, v)); }
  friend ddd operator>=(const difference& c, tCstr v) { return ddd(mkDiffCstr(c.pos(), c.neg(), GEQ, v));}

  // Constructors for difference constraints such as x<y and x<=y
  friend ddd operator<(tCstr v, const difference& c)  { return c>v; }
  friend ddd operator<=(tCstr v, const difference& c) { return c>=v; }
  friend ddd operator==(tCstr v, const difference& c) { return c==v; }
  friend ddd operator!=(tCstr v, const difference& c) { return c!=v; }
  friend ddd operator>(tCstr v, const difference& c)  { return c<v; }
  friend ddd operator>=(tCstr v, const difference& c) { return c<=v; }

  friend ddd operator<(const real& x, const real& y)  { return x-y<0; }
  friend ddd operator<=(const real& x, const real& y) { return x-y<=0; }
  friend ddd operator==(const real& x, const real& y) { return x-y==0; }
  friend ddd operator!=(const real& x, const real& y) { return x-y!=0; }
  friend ddd operator>=(const real& x, const real& y) { return x-y>=0; }
  friend ddd operator>(const real& x, const real& y)  { return x-y>0; }

  // Boolean operators on ddd's (the priority is the usual i C++: !, >>, ==, &, |)
  friend ddd operator! (const ddd& u);               // Negation
  friend ddd operator>>(const ddd& u, const ddd& v); // Implication
  friend ddd operator==(const ddd& u, const ddd& v); // Biimplication
  friend ddd operator!=(const ddd& u, const ddd& v); // not-Biimplication
  friend ddd operator& (const ddd& u, const ddd& v); // Conjunction
  friend ddd operator| (const ddd& u, const ddd& v); // Disjunction
  friend ddd operator- (const ddd& u, const ddd& v); // Difference
  ddd operator&=(const ddd& v); // Conjunction and assign
  ddd operator|=(const ddd& v); // Disjunction and assign
  ddd operator-=(const ddd& v); // Difference and assign

  // Assignment with integer
  friend ddd operator==(const enumerator& e, unsigned int state);

  // Quantifiers
  friend ddd Forall(const var& x, const ddd& u);
  friend ddd Exists(const var& x, const ddd& u);
  friend ddd Forall(const varlist& v, const ddd& u);
  friend ddd Exists(const varlist& v, const ddd& d);

  // Assignment of non-Boolean variables u[x:=y+c], and Boolean
  // variables u[x:=b]
  friend ddd Assign(const ddd& u, const real& x, const real& y, tCstr c=0);
  friend ddd Assign(const ddd& u, const boolean& x, unsigned short int b);

  // Replacement of variables u[x/y]
  friend ddd Replace(const ddd& u, const var& x, const var& y);
  friend ddd Replace(const ddd& d, const varlist& v1, const varlist& v2);

  // Functional properties
  friend bool Tautology(const ddd& u);
  friend bool Satisfiable(const ddd& u);
  friend bool Unsatisfiable(const ddd& u);
  friend bool Falsifiable(const ddd& u);
  friend bool Equivalent(const ddd& u, const ddd& v);
  friend bool Consequence(const ddd& u, const ddd& v);

  // Return a semantically equivalent ddd with no infeasible paths
  friend ddd PathReduce(const ddd& u);

  // Merge path that are convex
  friend ddd Merge(const ddd& u);

  // Merge path that are convex
  friend ddd Saturate(const ddd& u);

  // Return the convex hull of u
  friend ddd Hull(const ddd& u);

  // Print satisfying variable assignment
  friend void Anysat(const ddd& u);
  
  // Count the number of 1-path in u
  friend double PathCount(const ddd& u);

  // Count the number of nodes in u
  friend unsigned int NodeCount(const ddd& u);

  // Count the number of variables in u
  friend unsigned int VarCount(const ddd& u);

  // Return true if x is free in u
  friend bool IsFreeVar(const var& x, const ddd& u);

  // Print u in DNF, z is interpreted as zero
  friend void PrintDNF(const ddd& u, const real& z);

  // Print u in INF
  friend void PrintINF(const ddd& u);

  // Print u in INF (using \/ and /\ whenever possible and ITE form otherwise)
  friend void PrintINFpretty(const ddd& u);

  // Save/restore a ddd to a text file in a very simple format (might
  // have exponential runtime because it enumerates all paths)
  friend void SaveDDD(const char* name, const ddd& u);
  friend ddd  RestoreDDD(const char* name);

  // Various graph/plot functions
  friend void SaveGraph(const char* dotname, const ddd& u);
  friend void ViewGraph(const ddd& u);
  friend void SavePlot(const char* texname,
		       const real& z, const real& x, const real& y, const ddd& u);
  friend void ViewPlot(const real& z, const real& x, const real& y, const ddd& u);
  
  friend void SetGraphOptions(bool asDot, bool graphNumbering);
  friend void SetGraphName(const char* name, ...);
  friend void SetPlotWindow(int xMin, int yMin, int xMax, int yMax);
  friend void SetPlotSize(int width, int height);
  friend void SetPlotAxis(bool showAxis, bool showNumbers,
  			 bool showTicks, bool showAxisInFront);
  friend void SetPlotEdges(bool plotEdges);
  friend void SetPlotColours(tColour fillColour, tColour edgeColour);
  friend void SetPlotSaveOptions(bool saveAsArticle);
  friend void SetPlotPsCode(const char* psCode);

private:
  // Mega-hack: this function must be called with y==42.
  ddd(tDDD r,tDDD y)      { assert(y==42); m_r = r; }

  // Not implemented
  ddd& operator=(tDDD);   

  // The root node
  tDDD m_r;

public:
  // Only used to create the two constants.
  friend ddd ddd_true(void);
  friend ddd ddd_false(void);
};

#endif


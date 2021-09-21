#include "constraint-system.h"
#include "table.h"
#include "memory.h"
#include "variable.h"
#include "io.h"

/* Initial size of the constraint-system stack */
#define INITIAL_SIZE   2

/* The index of the i'th element in the current distance vector */
#define CURRENT(i)     ((m_maxVar*m_current) + i)

/* Weights in the graph are of type tCstrOp, but adding two such
   constraints will not work, since (<,a)+(<,b) = (<,a+b).  Thus we
   cannot detect negative cycles in graphs like [(x,y,(<,0)),
   (y,x,(<,0)].  We can think of (<,a) as (<=,a-eps), and then 
   (<,a)+(<,b) = (<=,a-eps) +  (<=,b-eps) =  (<=,a+b-2*eps).  With
   this encoding we can detect cycles. */
typedef struct {
  /* The number of epsilons to subtract from value */
  unsigned int eps;

  /* The constraint value */
  tCstr value;
} tCstrEps;

/* The stack of distance vectors (stored in a row-major manner). */
static tCstrEps* m_distance;

/* The number of m_distance vectors currently allocated */
static unsigned int m_size;

/* The stack index of the current distance vector */
static unsigned int m_current;

/* Set of variables used to count the number of variables on a path */
tVar* m_vars;
tVar m_count;

/* Solution to a constraint-system. */
static tCstrOp* m_solution;

void initCstrSystem(void) {
  m_size = INITIAL_SIZE;
  m_current = 0;
  m_distance = allocMem(m_distance, m_maxVar * m_size * sizeof(tCstrEps));
  m_vars = allocMem(m_vars, m_maxVar * sizeof(tVar));
  m_count = 0;
  m_solution = allocMem(m_solution, m_maxVar * sizeof(tCstrOp));
}

void reinitCstrSystem(void) {
  doneCstrSystem();
  initCstrSystem();
}

void doneCstrSystem(void) {
  PRECONDITION(m_current==0);
  PRECONDITION(m_count==0);
  freeMem(m_distance);
  freeMem(m_vars);
  freeMem(m_solution);
}

static __inline__ void resetDistance(void) {
  /* Reset the current distance vector to zero */
  setMem(&(m_distance[CURRENT(0)]), 0, m_maxVar * sizeof(tCstrEps));
}

static __inline__ void resetVarSet(void) {
  /* Reset the number of variables */
  setMem(m_vars, 0, m_maxVar * sizeof(tVar));
  assert(m_count==0);
}

void resetCstrSystem(void) {
  resetDistance();
  resetVarSet();
}

void pushCstrSystem(void) {
  if (m_current==m_size-1) {
    /* Double the size */
    m_size *= 2;
    m_distance = reallocMem(m_distance, m_maxVar * m_size * sizeof(tCstrEps));
  }
  /* Copy the elements from the current working distance vector to the
     next */
  copyMem(&(m_distance[(m_current+1)*m_maxVar]),
	  &(m_distance[(m_current)*m_maxVar]),
	  sizeof(tCstrEps)*m_maxVar);
  /* Make the next the current working distance vector */
  m_current++;
}

void popCstrSystem(void) {
  m_current--;
  assert(m_current>=0);
}

/* Start at v and follow the marked sons until a terminal node is
   reached or a node with no marked sons.  Relax all edges on the way
   down.  Returns true if the were made any relaxations. */
static __inline__ bool relaxPath(tDDD u, tDDD v) {
  bool changed = false;
  while (u!=v) {
    if (typeOfVar(pos(u))==BOOLEAN) {
      u = path(u) ? high(u) : low(u);
    }
    else {
      tVar p,n;
      tCstrOp c;
      tCstrEps d;

      if (path(u)==0) {
	c = negCstrOp(cstrOp(u));
	p = neg(u);
	n = pos(u);
	u = low(u);
      }
      else {
	c = cstrOp(u);
	p = pos(u);
	n = neg(u);
	u = high(u);
      }
      /*  1&(~c) is a powerhack (should be opOfCstrOp(c)==COMP_LESS ? 1 : 0) */
      d.eps = m_distance[CURRENT(n)].eps + (1&(~c));
      d.value = m_distance[CURRENT(n)].value + valueOfCstrOp(c);

      if ((d.value< m_distance[CURRENT(p)].value) ||
	  (d.value==m_distance[CURRENT(p)].value && d.eps > m_distance[CURRENT(p)].eps)) {
	m_distance[CURRENT(p)] = d;
	changed = true;
      }
    }
  }
  return changed;
}

bool isFeasibleCstrSystem(tDDD u, tDDD v) {
  int i = m_count;
  bool changed;

  /* Relax at most i+1 times and stop if there are no more changes */
  do {
    changed = relaxPath(u,v);
  } while (changed && i-->=0);

  /* If the last relaxation of the edges made any changes, we can keep
     on relaxing, so the constraint system has no solution. */
  return !changed;
}

static bool allInfeasibleAux(tDDD u, tDDD v, tDDD t) {
  bool ok;
  if (v<=1 && v!=t) {
    return true;
  }
  if (v<=1) {
#ifdef INCR_BELLMANFORD
    /* Use the current distance vector */
#else
    resetDistance();
#endif
    ok = isFeasibleCstrSystem(u,v); 
    return !ok;
  }
  else {
#ifdef INCR_BELLMANFORD
    pushCstrSystem();
#endif
    if (typeOfVar(pos(v))==REAL)
      addVarsCstrSystem(vars(v));
    assert(path(v)==0);
    ok = allInfeasibleAux(u, low(v), t);
#ifdef INCR_BELLMANFORD
    popCstrSystem();
#endif
    if (ok) {
      setPath(v);
      ok = allInfeasibleAux(u, high(v), t);
      resetPath(v);
    }
    if (typeOfVar(pos(v))==REAL)
      removeVarsCstrSystem(vars(v));
    return ok;
  }
}

bool allInfeasible(tDDD u, tDDD t) {
  PRECONDITION(t<=1);
  PRECONDITION(isValidDDD(u));
  return allInfeasibleAux(u,u,t);
}

static bool existsFeasibleAux(tDDD u, tDDD v, tDDD t) {
  bool ok;
  if (v<=1 && v!=t) {
    return false;
  }
  if (v<=1) {
#ifdef INCR_BELLMANFORD
    /* Use the current distance vector */
#else
    resetDistance();
#endif
    ok = isFeasibleCstrSystem(u,v); 
    return ok;
  }
  else {
#ifdef INCR_BELLMANFORD
    pushCstrSystem();
#endif
    if (typeOfVar(pos(v))==REAL)
      addVarsCstrSystem(vars(v));
    assert(path(v)==0);
    ok = existsFeasibleAux(u, low(v), t); 
#ifdef INCR_BELLMANFORD
    popCstrSystem();
#endif
    if (!ok) {
      setPath(v);
      ok = existsFeasibleAux(u, high(v), t);
      resetPath(v);
    }
    if (typeOfVar(pos(v))==REAL)
      removeVarsCstrSystem(vars(v));
    return ok;
  }
}

bool existsFeasible(tDDD u, tDDD t) {
  PRECONDITION(t<=1);
  PRECONDITION(isValidDDD(u));
  return existsFeasibleAux(u,u,t);
}


static __inline__ void saveSolutionCstrSystem(tDDD u, tDDD v) {
  tVar i;
  /* Set all difference constraint variables to the minimal distance,
     or oo if the variable is Boolean or not in the path (i.e. a don't
     care). */
  for (i=0; i<m_curVar; i++) {
    if (typeOfVar(i)==REAL && m_vars[i]>0)
      m_solution[i] = m_distance[CURRENT(i)].eps>0
	? mkCstrOpLe(m_distance[CURRENT(i)].value)
	: mkCstrOpLeq(m_distance[CURRENT(i)].value);
    else 
      m_solution[i] = oo;
  }

  /* Set all Boolean variables to true (1) or false (0) if in the path. */
  while (u!=v) {
    i = pos(u);
    if (typeOfVar(i)==BOOLEAN && kindOfVar(i)==USER) {
      PRECONDITION(typeOfVar(neg(u))==BOOLEAN);
      m_solution[i] = path(u) ? 1 : 0;
    }
    u = path(u) ? high(u) : low(u);
  }
}

static bool saveFeasibleAux(tDDD u, tDDD v, tDDD t) {
  bool ok;
  if (v<=1 && v!=t) {
    return false;
  }
  else if (v<=1) {
#ifdef INCR_BELLMANFORD
    /* Use the current distance vector */
#else
    resetDistance();
#endif
    ok = isFeasibleCstrSystem(u,v); 
    if (ok)
      saveSolutionCstrSystem(u,v);
    return ok;
  }
  else {
#ifdef INCR_BELLMANFORD
    pushCstrSystem();
#endif
    if (typeOfVar(pos(v))==REAL)
      addVarsCstrSystem(vars(v));
    assert(path(v)==0);
    ok = saveFeasibleAux(u,low(v),t); 
#ifdef INCR_BELLMANFORD
    popCstrSystem();
#endif
    if (!ok) {
      setPath(v);
      ok = saveFeasibleAux(u,high(v),t);
      resetPath(v);
    }
    if (typeOfVar(pos(v))==REAL)
      removeVarsCstrSystem(vars(v));
    return ok;
  }
}

bool hasFeasibleSolution(tDDD u, tDDD t) {
  PRECONDITION(t<=1);
  PRECONDITION(isValidDDD(u));
  return saveFeasibleAux(u,u,t);
}

void printFeasibleSolution(void) {
  tVar v;
  for (v=0; v<m_curVar; v++) {
    if (m_solution[v]!=oo) {
      printVar(v);
      printMsg(" = ");
      if (typeOfVar(v)==BOOLEAN) {
	PRECONDITION(kindOfVar(v)==USER);
	PRECONDITION(kindOfVar(v-1)==INTERNAL);
	PRECONDITION(m_solution[v]==0 || m_solution[v]==1);
	printMsg("%s", m_solution[v] ? "true" : "false");
      }
      else {
	PRECONDITION(typeOfVar(v)==REAL);
	printMsg("%d%s", valueOfCstrOp(m_solution[v]),
		 opOfCstrOp(m_solution[v])==COMP_LESS ? " - e":"");
      }
      printMsg("\n");
    }
  }
}

#ifndef CONSTRAINT_SYSTEM_H
#define CONSTRAINT_SYSTEM_H

#include "types.h"
#include "variable.h"

/* The constraint system module implements a data structure to solve
   difference constraint systems over integer or real variables.  The
   function isFeasibleCstrSystem(tDDD u, tDDD v) returns true if the
   difference constraint system consisting of the difference
   constraints along the BPF path from u to v (v exclusive) is
   feasible.  The function uses an algorithm similar to Bellman-Ford's
   single source shortest path algorithm to determine feasibility.  In
   DEBUG mode the result is checked against a Floyd-Warshall algorithm
   (assumed to be easier to understand than the Bellman-Ford algorithm
   and thus more likely to be correct). */

void initCstrSystem(void);
void reinitCstrSystem(void);
void doneCstrSystem(void);

/* The path from u to v should be a BPF path (ie. when path(x) is 0
   low(x) is followed, otherwise high(x) is followed).

   The constraints on the path from u to v form a constraint system,
   and the function returns true if the constraint system is feasible.
   
   (The function uses the Bellman-Ford algorithm (CLR, p. 532). The
   constraints on the path from u to v are interpreted as edges in a
   graph, such that a contraint xj - xi op c gives rise to an edge
   from xi to xj with weight (op,c).  If the graph has a no
   negative-weight cycle, the function returns true.) */
bool isFeasibleCstrSystem(tDDD u, tDDD v);

/* Return true if all t-paths (0 or 1) are infeasible */
bool allInfeasible(tDDD u, tDDD t);

/* Return true if there exists a t-path (0 or 1) that is feasible */
bool existsFeasible(tDDD u, tDDD t);

/* Return true if there exists a t-path (0 or 1) that is feasible and
   save that feasible solution */
bool hasFeasibleSolution(tDDD u, tDDD t);

/* Print the saved solution */
void printFeasibleSolution(void);

/* Reset the current distance vector to zero */
void resetCstrSystem(void);

/* Makes a copy of the current solution to the constraint system and
   pushes it on a stack, such that it becomes the current constraint
   system.  All elements in the current distance vector are copied to
   the new vector, so that results may be reused. */
void pushCstrSystem(void);

/* Pop the current solution to the constraint system and make the
   previous constraint system the current. */
void popCstrSystem(void);

/* Add two variables to the current constraint system. */
static __inline__ void addVarsCstrSystem(tVars v) ATTR_UNUSED;

/* Remove two variables from the current constraint system. */
static __inline__ void removeVarsCstrSystem(tVars v) ATTR_UNUSED;


/* Inline definitions ------------------------------------------------------ */

extern tVar* m_vars;
extern tVar m_count;

static __inline__ void addVarsCstrSystem(tVars v) {
  PRECONDITION(isValidVars(v));
  if (0==m_vars[posOfVars(v)]++)
    m_count++;
  if (0==m_vars[negOfVars(v)]++)
    m_count++;
}

static __inline__ void removeVarsCstrSystem(tVars v) {
  PRECONDITION(isValidVars(v));
  PRECONDITION(m_vars[posOfVars(v)]>0);
  PRECONDITION(m_vars[negOfVars(v)]>0);
  if (0==--m_vars[posOfVars(v)])
    m_count--;
  if (0==--m_vars[negOfVars(v)])
    m_count--;
}

#endif

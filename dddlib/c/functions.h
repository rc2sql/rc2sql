#ifndef FUNCTIONS_H
#define FUNCTIONS_H

#include "types.h"
#include "table.h"

/* Create a node u with positive variable p, negative variable v and
   constraint c, and low and high sons l and h, respectively.  The
   following must hold:
   1. vars(u)< vars(l) or cstrOp(u)<cstrOp(l)     Orderness req #1
   2. vars(u)< vars(h)                            Orderness req #2
   3. p > n                                       Normalization req */
static __inline__ tDDD mk(tVars v, tCstrOp c, tDDD h, tDDD l) ATTR_UNUSED;

BEGIN_EXPORT
/* The DDDs representing false and true, respectively */
extern const tDDD ff;
extern const tDDD tt;

/* Construct the DDD for each of the six types of difference
   constraints 'p-n o c', where p and n are variables created with
   mkVar, o is a relational comparator, and c is a constant.  The
   variables must be different. */
tDDD mkDiffCstr(tVar p, tVar n, tComp o, tCstr c);

/* Construct the DDD for the Boolean variable b.  */
tDDD mkBoolVar(tVar b);

/* Combine DDDs with Boolean connectives */
tDDD nor(tDDD u, tDDD v);
tDDD nimp(tDDD u, tDDD v);
tDDD xor(tDDD u, tDDD v);
tDDD nand(tDDD u, tDDD v);
tDDD and(tDDD u, tDDD v);
tDDD biimp(tDDD u, tDDD v);
tDDD imp(tDDD u, tDDD v);
tDDD or(tDDD u, tDDD v);
tDDD not(tDDD);

/* Existential quantification */
tDDD exists(tVar x, tDDD u);

/* Universal quantification */
tDDD forall(tVar x, tDDD u);

/* Assign x to y+c in u, where x and y are non-Boolean variables
   (i.e., u[x <- y+c]). */
tDDD assign(tDDD u, tVar x, tVar y, tCstr c);

/* Assign x to b (true or false) in u, where x is a Boolean
   variable (i.e., u[x <- b]). */
tDDD assignBool(tDDD u, tVar x, unsigned short int b);

/* Replace all occurrences of y in u by x (i.e., u[x/y]). */
tDDD replace(tDDD u, tVar x, tVar y);

/* True iff u is true in all assignments */
bool tautology(tDDD u);

/* True iff u is true in some assignment */
bool satisfiable(tDDD u);

/* True iff u is false in all assignments */
bool unsatisfiable(tDDD u);

/* True iff u is false in some assignment */
bool falsifiable(tDDD u);

/* True iff u and v are semantically equivalent */
bool equivalent(tDDD u, tDDD v);

/* True iff v is a logical consequence of u */
bool consequence(tDDD u, tDDD v);

/* Remove all infeasible paths in u */
tDDD pathReduce(tDDD u);

/* Remove all mergeable vertices in u */
tDDD merge(tDDD u);

/* Saturate u */
tDDD saturate(tDDD u);

/* Return the convex hull of u (i.e., the minimum and maximum
   differences between all variables in u). */
tDDD hull(tDDD u);

/* Print a satisfying variable assignment of u (if one exists). */
void anysat(tDDD u);

/* Count the number of vertices in u */
unsigned int nodeCount(tDDD u);

/* Count the number of 1-paths in u */
double pathCount(tDDD u);

/* Count the number of different variables in u. */
unsigned int varCount(tDDD u);

/* Returns true if the variable x is free in u */
bool isFreeVar(tVar x, tDDD u);
END_EXPORT

static __inline__ tDDD mk(tVars v, tCstrOp c, tDDD h, tDDD l) {
  /* Assert that the parameters are not garbage */
  PRECONDITION(isValidRefDDD(l));
  PRECONDITION(isValidRefDDD(h));

  PRECONDITION(isValidVar(posOfVars(v)));
  PRECONDITION(isValidVar(negOfVars(v)));

  /* Orderdness req #1 */
  PRECONDITION(compareVarsCstrOp(v,c,vars(l),cstrOp(l)) & (VAR_LE|CSTR_LE));

  /* Orderdness req #2 */
  PRECONDITION(compareVarsCstrOp(v,c,vars(h),cstrOp(h)) & VAR_LE);

  /* Normalized req */
  PRECONDITION(posOfVars(v)>negOfVars(v));

  /* Reducedness req #1 (l==h) and #3 */
  if (l==h || (v==vars(l) && h==high(l)))
    return l;

  /* Reducedness req #2 */
  return insertTable(v,c,l,h);
}


#endif

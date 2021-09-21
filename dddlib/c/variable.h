#ifndef VARIABLES_H
#define VARIABLES_H

#include "types.h"

/* The largest number of variables the package can handle */
#define VAR_MAX 0x0000ffff

BEGIN_EXPORT
/* A variable is represented by an integer. */
typedef short unsigned int tVar; 

/* Create an integer- or real-valued variable with the given name.
   Variable names are used in pretty printing DDDs, so names should
   differ.  The variable ordering is the order of creation. */
tVar mkVar(const char* name);

/* Create a Boolean variable with the given name (like mkVar). */
tVar mkBool(const char* name);

/* The name of a variable */
const char* nameOfVar(tVar v);

/* Constants in upper bounds are signed integers.  Because of the
   encoding of upper bouds, an constant should only contain 31 bits
   (1 sign bit and 30 data bits). */
typedef int tCstr;

#define CSTR_MIN (-((1<<30)-1))
#define CSTR_MAX ((1<<30)-1)

/* Relational operators used in difference constraint. */
typedef enum {
  EQ  = 0,     /* == */
  NEQ = 1,     /* != */
  LEQ = 2,     /* <= */
  GEQ = 3,     /* >= */
  LE  = 4,     /* <  */
  GR  = 5      /* >  */
} tComp;

/* Set (x,y) as the last variable pair in the ordering */
void appendRealToOrder(tVar x, tVar y);

/* Set x as the last variable pair in the ordering */
void appendBoolToOrder(tVar x);

/* Set (x,y) as the first variable pair in the ordering */
void prependRealToOrder(tVar x, tVar y);

/* Set x as the first variable pair in the ordering */
void prependBoolToOrder(tVar x);

/* Order the (remainig) variable pairs */
void appendIncrLexToOrder(void);
void prependIncrLexToOrder(void);
void appendDecrLexToOrder(void);
void prependDecrLexToOrder(void);

/* Clear the order of all variables */
void clearOrder(void);
void printOrder(void);

END_EXPORT

/* A constraint variable is a pair (negative variable, positive
   variable) and is encoded in a single 32-bit unsigned int */
typedef unsigned int tVars;

int orderOfPair(tVar x, tVar y);
int orderOf(tVars v);

void initOrdering(void);
void reinitOrdering(void);
void doneOrdering(void);
void infoOrdering(void);

/* The order of variable-pairs as an array (with length 'length') of
   pairs */
const tVars* const getOrder(int* length);

void initVariable(void);
void reinitVariable(void);
void doneVariable(void);
void infoVariable(void);

/* The two types of variables */
typedef enum { BOOLEAN, REAL } tVarType;

/* The two kinds of variables */
typedef enum { INTERNAL, USER } tVarKind;

/* The type and kind of a variable */
tVarType typeOfVar(tVar v);
tVarKind kindOfVar(tVar v);

/* Predicate to determine whether v is a valid variable */
bool isValidVar(tVar v);

/* Predicate to determine whether v is a valid variable-pair */
bool isValidVars(tVars v);

/* Construct a constraint variable from two integer variables */
static __inline__ tVars mkVars(tVar p, tVar n) ATTR_UNUSED ATTR_CONST;

/* Get the identifier of the positive constraint in a variable */
static __inline__ tVar posOfVars(tVars v) ATTR_UNUSED ATTR_CONST;

/* Get the identifier of the negative constraint in a variable */
static __inline__ tVar negOfVars(tVars v) ATTR_UNUSED ATTR_CONST;

/* Constraint comparators (Note: the order is important and
   should not be changed) */
typedef enum {
  COMP_LESS   = 0,
  COMP_LESSEQ = 1
} tOp;

/* A constraint (tCstrOp) consists of two pieces of information, the
   constraint value (tCstr) and the comparator (tOp).  There are two
   comparators: < and <=.  The 31 most significant bits are used for
   the constraint value.  The least significant bit is the comparator
   (0 means <, and 1 means <=).  Integer constraints only use <=,
   since '< a' is the same as '< = a-1'.

   For real constraints:
     (<, a) + (<, b) = (<, a + b)
     (<=,a) + (<, b) = (<, a + b)
     (<, a) + (<=,b) = (<, a + b)
     (<=,a) + (<=,b) = (<=,a + b)
            - (<, a) = (<=,-a)
            - (<=,a) = (<, -a)

   For integer constraints:
     (<=,a) + (<=,b) = (<=,a + b)
            - (<=,a) = (<=,-a-1)
*/
typedef int tCstrOp;

extern const tCstrOp oo;

static __inline__ tCstrOp mkCstrOpLeq(tCstr v) ATTR_UNUSED ATTR_CONST;
static __inline__ tCstrOp mkCstrOpLe(tCstr v) ATTR_UNUSED ATTR_CONST;

static __inline__ tCstr valueOfCstrOp(tCstrOp c) ATTR_UNUSED ATTR_CONST;
static __inline__ tOp  opOfCstrOp(tCstrOp c) ATTR_UNUSED ATTR_CONST;

static __inline__ tCstrOp addCstrOp(tCstrOp c1, tCstrOp c2) ATTR_UNUSED ATTR_CONST;
static __inline__ tCstrOp negCstrOp(tCstrOp c) ATTR_UNUSED ATTR_CONST;

/* The different possibilities when comparing variables and difference
   constraints (do not change the order). */
typedef enum {
  VAR_LE  =  1,
  VAR_GR  =  2,
  CSTR_LE =  4,
  CSTR_EQ =  8,
  CSTR_GR = 16
} tVarCstrComp;

static __inline__ tVarCstrComp
compareVarsCstrOp(tVars v1, tCstrOp c1, tVars v2, tCstrOp c2) ATTR_UNUSED ATTR_CONST;


/* Validity functions */
static __inline__ bool isValidCstr(tCstr c) ATTR_UNUSED ATTR_CONST; 
static __inline__ bool isValidComp(tComp c) ATTR_UNUSED ATTR_CONST; 
static __inline__ bool isValidOp(tOp o) ATTR_UNUSED ATTR_CONST; 
static __inline__ bool isValidCstrOp(tCstrOp c) ATTR_UNUSED ATTR_CONST; 


/* Inline definitions ------------------------------------------------------ */

static __inline__ tCstrOp mkCstrOpLeq(tCstr v) {
  assert(isValidCstr(v));
  return (v<<1) | COMP_LESSEQ;
}

static __inline__ tCstrOp mkCstrOpLe(tCstr v) {
#ifdef INTEGER_SEMANTICS
  assert(isValidCstr(v-1));
  return ((v-1)<<1) | COMP_LESSEQ;
#else
  assert(isValidCstr(v));
  return (v<<1) | COMP_LESS;
#endif
}

static __inline__ tCstr valueOfCstrOp(tCstrOp c) {
#ifdef SHR_IS_ARITHMETIC
  return c>>1;
#else
  return (c>>1) | (c&0x80000000);
#endif
}

static __inline__ tOp opOfCstrOp(tCstrOp c) {
#ifdef INTEGER_SEMANTICS
  assert((c&1)==COMP_LESSEQ);
#endif
  return c&1;
}

static __inline__ tCstrOp addCstrOp(tCstrOp c1, tCstrOp c2)  {
  assert(isValidCstr(valueOfCstrOp(c1)+valueOfCstrOp(c2)));
#ifdef INTEGER_SEMANTICS
  assert(opOfCstrOp(c1)==COMP_LESSEQ && opOfCstrOp(c2)==COMP_LESSEQ);
  return c1+c2-1;
#else
  return c1+c2-(1&(c1|c2));
#endif
}

static __inline__ tCstrOp negCstrOp(tCstrOp c)  {
#ifdef INTEGER_SEMANTICS
  assert(opOfCstrOp(c)==COMP_LESSEQ);
  return -c;
#else
  return -c+1;
#endif
}

static __inline__ tVarCstrComp
compareVarsCstrOp(tVars v1, tCstrOp c1, tVars v2, tCstrOp c2) {
  if (orderOf(v1) < orderOf(v2))
    return VAR_LE;
  else if (orderOf(v1) > orderOf(v2))
    return VAR_GR;
  else if (c1 < c2)
    return CSTR_LE;
  else if (c1 > c2)
    return CSTR_GR;
  else
    return CSTR_EQ;
}

static __inline__ bool isValidCstr(tCstr c) {
  return c>=CSTR_MIN && c<=CSTR_MAX;
};

bool isValidComp(tComp c) {
  return c==EQ || c==NEQ || c==LEQ || c==GEQ || c==LE || c==GR;
};

bool isValidOp(tOp o) {
  return o==COMP_LESS || o==COMP_LESSEQ;
};

static __inline__ bool isValidCstrOp(tCstrOp c) {
#ifdef INTEGER_SEMANTICS
  return isValidCstr(valueOfCstrOp(c)) && opOfCstrOp(c)==COMP_LESSEQ;
#else
  return isValidCstr(valueOfCstrOp(c));
#endif
}

/* Inline definitions ------------------------------------------------------ */

static __inline__ tVars mkVars(tVar p, tVar n) {
  assert(p<(1<<16));
  assert(n<(1<<16));
  assert(p!=n);
  return (n<<16) | p;
}

static __inline__ tVar posOfVars(tVars v) {
  return v & 0x0000ffff;
}

static __inline__ tVar negOfVars(tVars v) {
  return v>>16;
}

/* The current maximal number of variables - set by init */
extern tVar m_maxVar;

/* The current number of variables - controlled by mkVar */
extern tVar m_curVar;

#endif

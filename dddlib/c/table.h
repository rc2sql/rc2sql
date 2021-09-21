#ifndef TABLE_H
#define TABLE_H

#include "types.h"
#include "variable.h"

BEGIN_EXPORT
/* Increment u's reference count (protecting it from garbage
   collection). */
void incRef(tDDD u);

/* Decrement u's reference count (making it recyclable if not
   referenced anymore). */
void decRef(tDDD u);

/* Type of user-defined garbage collection functions. */
typedef void (*tPreGC)(void);
void set_pre_gc(tPreGC f);
END_EXPORT

void initTable(void);
void reinitTable(void);
void doneTable(void);
void infoTable(void);

/* Get various fields of a DDD node */
static __inline__ tVars vars(tDDD u) ATTR_UNUSED;
static __inline__ tVar  pos(tDDD u) ATTR_UNUSED;
static __inline__ tVar  neg(tDDD u) ATTR_UNUSED;
static __inline__ tCstrOp cstrOp(tDDD u) ATTR_UNUSED;
static __inline__ tDDD low(tDDD u) ATTR_UNUSED;
static __inline__ tDDD high(tDDD u) ATTR_UNUSED;
static __inline__ tDDD next(tDDD u) ATTR_UNUSED;
static __inline__ unsigned int mark(tDDD u) ATTR_UNUSED;
static __inline__ unsigned int path(tDDD u) ATTR_UNUSED;
static __inline__ unsigned int ref(tDDD u) ATTR_UNUSED;
static __inline__ tDDD hash(tDDD u) ATTR_UNUSED;

/* Set various fields of a DDD node */
static __inline__ void setMark(tDDD u) ATTR_UNUSED;
static __inline__ void resetMark(tDDD u) ATTR_UNUSED;
static __inline__ void setPath(tDDD u) ATTR_UNUSED;
static __inline__ void resetPath(tDDD u) ATTR_UNUSED;
static __inline__ void inlineIncRef(tDDD u) ATTR_UNUSED;
static __inline__ void inlineDecRef(tDDD u) ATTR_UNUSED;

static __inline__ void markNode(tDDD P) ATTR_UNUSED;
static __inline__ void unmarkNode(tDDD P) ATTR_UNUSED;

/* Insert node in the table */
tDDD insertTable(tVars v, tCstrOp c, tDDD l, tDDD h);

/* Function to determine validity of DDD's (should only be used in
   assertions). */
bool isValidDDD(tDDD u) ATTR_UNUSED;
bool isValidRefDDD(tDDD u) ATTR_UNUSED;
bool isTerm(tDDD u) ATTR_UNUSED;
bool isNonTerm(tDDD u) ATTR_UNUSED;


/* Inline definitions ------------------------------------------------------ */

/* The number of bits used in reference counts and marks and path bits */
#define REF_BITS  (20)
#define PAD_BITS  (10)
#define MARK_BITS (1)
#define PATH_BITS (1)
#define MAX_REF   ((1<<REF_BITS)-1)


/* The encoding of a ddd node.  Each ddd node takes up 28 bytes of memory. */
typedef struct {
  tVars vars;                   /* The variable (see variables.h) */

  unsigned int ref:  REF_BITS;  /* Reference counter */
  unsigned int pad:  PAD_BITS;  /* Unused */
  unsigned int mark: MARK_BITS; /* Mark field */
  unsigned int path: PATH_BITS; /* Mark field */

  tCstrOp cstrOp;               /* The constraint (see constraints.h) */
  tDDD low;                     /* Index of low son */
  tDDD high;                    /* Index of high son */
  tDDD next;                    /* Index of the next free node on the freelist,
				   or the next ddd in the hash chaining. */
  tDDD hash;                    /* Hashtable */
} tDDDNode;

extern tDDDNode* m_ddd;


static __inline__ tVars vars(tDDD u) {
  PRECONDITION(isValidDDD(u));
  return m_ddd[u].vars;
}
static __inline__ tVar pos(tDDD u) {
  return posOfVars(vars(u));
}
static __inline__ tVar neg(tDDD u) {
  return negOfVars(vars(u));
}

static __inline__ tCstrOp cstrOp(tDDD u) {
  PRECONDITION(isValidDDD(u));
  return m_ddd[u].cstrOp;
}

static __inline__ tDDD low(tDDD u) {
  PRECONDITION(isValidDDD(u) && isNonTerm(u));
  return m_ddd[u].low;
}

static __inline__ tDDD high(tDDD u) {
  PRECONDITION(isValidDDD(u) && isNonTerm(u));
  return m_ddd[u].high;
}

static __inline__ tDDD next(tDDD u) {
  return m_ddd[u].next;
}

static __inline__ unsigned int mark(tDDD u) {
  PRECONDITION(isValidDDD(u));
  return m_ddd[u].mark;
}

static __inline__ unsigned int path(tDDD u) {
  PRECONDITION(isValidDDD(u) && isNonTerm(u));
  return m_ddd[u].path;
}

static __inline__ unsigned int ref(tDDD u) {
  PRECONDITION(isValidDDD(u));
  return m_ddd[u].ref;
}

static __inline__ tDDD hash(tDDD u) {
  return m_ddd[u].hash;
}

static __inline__ void setMark(tDDD u) {
  PRECONDITION(isValidDDD(u));
  m_ddd[u].mark = 1;
}

static __inline__ void resetMark(tDDD u) {
  PRECONDITION(isValidDDD(u));
  m_ddd[u].mark = 0;
}

static __inline__ void setPath(tDDD u) {
  PRECONDITION(isValidDDD(u));
  m_ddd[u].path = 1;
}

static __inline__ void resetPath(tDDD u) {
  PRECONDITION(isValidDDD(u));
  m_ddd[u].path = 0;
}

static __inline__ void inlineIncRef(tDDD u) {
  PRECONDITION(isValidDDD(u));
  if (m_ddd[u].ref<MAX_REF)
    m_ddd[u].ref++;
}

static __inline__ void inlineDecRef(tDDD u) {
  PRECONDITION(isValidRefDDD(u));
  if (m_ddd[u].ref<MAX_REF)
    m_ddd[u].ref--;
}

#define EXCHANGE(x,t,p,q) x=t; t=p; p=q

static __inline void markNode(tDDD P) {
  tDDD Q, T;
  assert(P>1);
  assert(mark(P)==0);

  T = 0;

E4:
  Q = high(P);
  if (Q>1 && mark(Q)==0) {
    EXCHANGE(m_ddd[P].high,T,P,Q);
    goto E4;
  }
E5:
  Q = low(P);
  setMark(P);
  if (Q>1 && mark(Q)==0) {
    EXCHANGE(m_ddd[P].low,T,P,Q);
    goto E4;
  }
  while (T!=0) {
    Q = T;
    if (mark(Q)==0) {
      EXCHANGE(T,m_ddd[Q].high,P,Q);
      goto E5;
    }
    else {
      EXCHANGE(T,m_ddd[Q].low,P,Q);
    }
  }
}


static __inline void unmarkNode(tDDD P) {
  tDDD Q, T;
  assert(P>1);
  assert(mark(P)==1);

  T = 0;

E4:
  Q = high(P);
  if (Q>1 && mark(Q)==1) {
    EXCHANGE(m_ddd[P].high,T,P,Q);
    goto E4;
  }
E5:
  Q = low(P);
  resetMark(P);
  if (Q>1 && mark(Q)==1) {
    EXCHANGE(m_ddd[P].low,T,P,Q);
    goto E4;
  }
  while (T!=0) {
    Q = T;
    if (mark(Q)==1) {
      EXCHANGE(T,m_ddd[Q].high,P,Q);
      goto E5;
    }
    else {
      EXCHANGE(T,m_ddd[Q].low,P,Q);
    }
  }
}

#endif

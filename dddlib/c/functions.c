#include "functions.h"
#include "cache.h"
#include "constraint-system.h"
#include "matrix.h"

const tDDD ff = 0;
const tDDD tt = 1;

/* Construct the ddd for the difference constraint: p-n o c */
tDDD mkDiffCstr(tVar p, tVar n, tComp o, tCstr c) {
  tDDD tmp1, tmp2;

  /* Variables must differ */
  PRECONDITION(p!=n);

  /* Variables must be difference constraint variables */
  PRECONDITION(typeOfVar(p)==REAL);
  PRECONDITION(typeOfVar(n)==REAL);

  /* Type restrictions */
  PRECONDITION(isValidVar(p));
  PRECONDITION(isValidVar(n));
  PRECONDITION(isValidComp(o));
  PRECONDITION(isValidCstr(c));

  switch(o) {
  case EQ:    /* p-n<c -> 0,(p-n<=c -> 1,0) */
    if (p>n) {
      inlineIncRef(tmp1 = mk(mkVars(p,n), mkCstrOpLeq(c), 1, 0));
      tmp2 = mk(mkVars(p,n), mkCstrOpLe(c), 0, tmp1);
    }
    else {
      inlineIncRef(tmp1 = mk(mkVars(n,p), mkCstrOpLeq(-c), 1, 0));
      tmp2 = mk(mkVars(n,p), mkCstrOpLe(-c), 0, tmp1);
    }
    inlineDecRef(tmp1);
    return tmp2;
  case NEQ:   /* p-n<c -> 1,(p-n<=c -> 0,1) */
    if (p>n) {
      inlineIncRef(tmp1 = mk(mkVars(p,n), mkCstrOpLeq(c), 0, 1));
      tmp2 = mk(mkVars(p,n), mkCstrOpLe(c), 1, tmp1);
    }
    else {
      inlineIncRef(tmp1 = mk(mkVars(n,p), mkCstrOpLeq(-c), 0, 1));
      tmp2 = mk(mkVars(n,p), mkCstrOpLe(-c), 1, tmp1);
    }
    inlineDecRef(tmp1);
    return tmp2;
  case LEQ:   /* p-n <= c -> 1,0 */
    return p>n ? mk(mkVars(p,n), mkCstrOpLeq(c), 1, 0)
               : mk(mkVars(n,p), mkCstrOpLe(-c), 0, 1);
  case LE:    /* p-n < c  -> 1,0 */
    return p>n ? mk(mkVars(p,n), mkCstrOpLe(c), 1, 0)
               : mk(mkVars(n,p), mkCstrOpLeq(-c), 0, 1);
  case GEQ:   /* p-n < c  -> 0,1 */
    return p>n ? mk(mkVars(p,n), mkCstrOpLe(c), 0, 1)
               : mk(mkVars(n,p), mkCstrOpLeq(-c), 1, 0);
  case GR:    /* p-n <= c -> 0,1 */
    return p>n ? mk(mkVars(p,n), mkCstrOpLeq(c), 0, 1)
               : mk(mkVars(n,p), mkCstrOpLe(-c), 1, 0);
  default:
    assert(0);
    return 0;
  }
}


tDDD mkBoolVar(tVar b) {
  /* Variables must be difference constraint variables */
  PRECONDITION(b>0);
  PRECONDITION(typeOfVar(b)==BOOLEAN  && kindOfVar(b)==USER);
  PRECONDITION(typeOfVar(b-1)==BOOLEAN && kindOfVar(b-1)==INTERNAL);
  PRECONDITION(0==strcmp(nameOfVar(b-1),""));

  /* Create the DDD for the difference constraint: b-z < 0 */
  return mk(mkVars(b,b-1), mkCstrOpLe(0), 1, 0);
}

/* Binary operators that can be used in apply.  If the order is
   changed, also change the case 'if (u<=1 && v<=1)' in apply. */
typedef enum {
  OP_FALSE =  0,  /* 0000 false     */
  OP_NOR   =  1,  /* 0001 u nor v   */
  OP_NRIMP =  2,  /* 0010 u <-/- v  */
  OP_NP1   =  3,  /* 0011 not u     */
  OP_NIMP  =  4,  /* 0100 u -/-> v  */
  OP_NP2   =  5,  /* 0101 not v     */
  OP_XOR   =  6,  /* 0110 u xor v   */
  OP_NAND  =  7,  /* 0111 u nand v  */
  OP_AND   =  8,  /* 1000 u and v   */
  OP_BIIMP =  9,  /* 1001 u <--> v  */
  OP_P2    = 10,  /* 1010 v         */
  OP_IMP   = 11,  /* 1011 u ---> v  */
  OP_P1    = 12,  /* 1100 u         */
  OP_RIMP  = 13,  /* 1101 u <--- v  */
  OP_OR    = 14,  /* 1110 u or v    */
  OP_TRUE  = 15   /* 1111 true      */
} tBoolOp;


static tDDD applyNot(tDDD u) {
  tDDD r;
  if (u<=1) {
    r = 1&(~u);
  }
  else {
    tDDD l,h;

    if (containsApplyCache(&r,OP_NP1,u,0)) 
      return r;

    inlineIncRef(l = applyNot(low(u)));
    inlineIncRef(h = applyNot(high(u)));
    r = mk(vars(u),cstrOp(u),h,l);
    inlineDecRef(l);
    inlineDecRef(h);

    insertApplyCache(r,OP_NP1,u,0);
  }
  return r;
}

/* Returns the index of the ddd for u o v */
static tDDD applyRec(tBoolOp o, tDDD u, tDDD v) {
  tDDD r,l,h;

  if (u<=1 && v<=1)
    /* This relies on the enumeration of operators.  Sigh! */
    return 1&(o >> (u<<1) >> v);

  switch (o) {
  case OP_FALSE:
    return 0;
  case OP_NOR:
    if (u==1 || v==1) return 0;
    break;
  case OP_NRIMP:
    if (u==1 || v==0 || u==v) return 0;
    if (u==0) return v;
    break;
  case OP_NP1:
    return applyNot(u);
  case OP_NIMP:
    if (u==0 || v==1 || u==v) return 0;
    if (v==0) return u;
    break;
  case OP_NP2:
    return applyNot(v);
    break;
  case OP_XOR:
    if (u==0) return v;
    if (v==0) return u;
    if (u==v) return 0;
    break;
  case OP_NAND:
    if (u==0 || v==0) return 1;
    break;
  case OP_AND:
    if (u==0 || v==0) return 0;
    if (u==1) return v;
    if (u==v || v==1) return u;
    break;
  case OP_BIIMP:
    if (u==v) return 1;
    if (u==1) return v;
    if (v==1) return u;  
    break;
  case OP_P2:
    return v;
  case OP_IMP:
    if (u==0 || v==1 || u==v) return 1;
    if (u==1) return v;
    break;
  case OP_P1:
    return u;
  case OP_RIMP:
    if (u==1 || v==0 || u==v) return 1;
    if (v==1) return u;
    break;
  case OP_OR:
    if (u==1 || v==1) return 1;
    if (u==0) return v;
    if (u==v || v==0) return u;
    break;
  case OP_TRUE:
    return 1;
  }

  if (containsApplyCache(&r,o,u,v))
    return r;

  switch (compareVarsCstrOp(vars(u),cstrOp(u), vars(v),cstrOp(v))) {
  case VAR_LE:
    inlineIncRef(l = applyRec(o, low(u),  v));
    inlineIncRef(h = applyRec(o, high(u), v));
    r = mk(vars(u), cstrOp(u), h, l);
    break;
  case CSTR_LE:
    inlineIncRef(l = applyRec(o, low(u),  v));
    inlineIncRef(h = applyRec(o, high(u), high(v)));
    r = mk(vars(u), cstrOp(u), h, l);
    break;
  case CSTR_EQ:
    inlineIncRef(l = applyRec(o, low(u), low(v)));
    inlineIncRef(h = applyRec(o, high(u),high(v)));
    r = mk(vars(u), cstrOp(u), h, l);
    break;
  case CSTR_GR:
    inlineIncRef(l = applyRec(o, u,       low(v)));
    inlineIncRef(h = applyRec(o, high(u), high(v)));
    r = mk(vars(v), cstrOp(v), h, l);
    break;
  case VAR_GR:
    inlineIncRef(l = applyRec(o, u, low(v)));
    inlineIncRef(h = applyRec(o, u, high(v)));
    r = mk(vars(v), cstrOp(v), h, l);
    break;
  default:
    m_errorHandler(MSG_COMPARE_ERROR); /* The errorhandler should never return */
    exit(1);                           /* but the compiler cannot determine that */
  }
  inlineDecRef(l);
  inlineDecRef(h);
  insertApplyCache(r,o,u,v);
  return r;
}

tDDD apply(tBoolOp o, tDDD u, tDDD v) {
  return applyRec(o,u,v);
}

tDDD nor(tDDD u, tDDD v) {
  PRECONDITION(isValidRefDDD(u) && isValidRefDDD(v));
  return apply(OP_NOR,u,v);
}

tDDD nimp(tDDD u, tDDD v) {
  PRECONDITION(isValidRefDDD(u) && isValidRefDDD(v));
  return apply(OP_NIMP,u,v);
}

tDDD xor(tDDD u, tDDD v) {
  PRECONDITION(isValidRefDDD(u) && isValidRefDDD(v));
  return apply(OP_XOR,u,v);
}

tDDD nand(tDDD u, tDDD v) {
  PRECONDITION(isValidRefDDD(u) && isValidRefDDD(v));
  return apply(OP_NAND,u,v);
}

tDDD and(tDDD u, tDDD v) {
  PRECONDITION(isValidRefDDD(u) && isValidRefDDD(v));
  return apply(OP_AND,u,v);
}

tDDD biimp(tDDD u, tDDD v) {
  PRECONDITION(isValidRefDDD(u) && isValidRefDDD(v));
  return apply(OP_BIIMP,u,v);
}

tDDD imp(tDDD u, tDDD v) {
  PRECONDITION(isValidRefDDD(u) && isValidRefDDD(v));
  return apply(OP_IMP,u,v);
}

tDDD or(tDDD u, tDDD v) {
  PRECONDITION(isValidRefDDD(u) && isValidRefDDD(v));
  return apply(OP_OR,u,v);
}

tDDD not(tDDD u) {
  PRECONDITION(isValidRefDDD(u));
  return applyNot(u);
}


static __inline__ tDDD ite(tVars v, tCstrOp b, tDDD h, tDDD l) {
  tDDD r, tmp1, tmp2;

  PRECONDITION(isValidVars(v));
  PRECONDITION(isValidCstrOp(b));
  PRECONDITION(isValidRefDDD(h));
  PRECONDITION(isValidRefDDD(l));

  inlineIncRef(r = mk(v,b,1,0));
  inlineIncRef(tmp1 = and(r, h));  inlineDecRef(r);
  inlineIncRef(r = mk(v,b,0,1));
  inlineIncRef(tmp2 = and(r, l));  inlineDecRef(r);
  r = or(tmp1,tmp2);
  inlineDecRef(tmp1); inlineDecRef(tmp2);
  return r;
}

static __inline__ tDDD mkNorm(tVar x, tVar y, tCstrOp b) {
  if (x==y) {
    return b>=mkCstrOpLeq(0) ? 1 : 0;
  }
  else {
    return x>y ? mk(mkVars(x,y),b,1,0)
               : mk(mkVars(y,x),negCstrOp(b),0,1);
  }
}

static tDDD relax(tDDD u, tVar z, tVar x, tVar y, tCstrOp b) {
  PRECONDITION(isValidVar(z));
  PRECONDITION(isValidVar(x));
  PRECONDITION(isValidVar(y));
  PRECONDITION(isValidDDD(u));
  PRECONDITION(z==x || z==y);

  if (u<=1) {
    return u;
  }
  else {
    tDDD r,h,l;

    if (containsRelaxCache(&r,u,z,x,y,b))
      return r;

    PRECONDITION(!(pos(u)==x && neg(u)==y));

    if (pos(u)==y && neg(u)==x) {
      PRECONDITION(b>negCstrOp(cstrOp(u)));
      inlineIncRef(h = relax(high(u),z,x,y,b));
      inlineIncRef(l = low(u));
    }
    else if (neg(u) == z) {
      if (z==x) {
	inlineIncRef(l = mkNorm(pos(u),y,addCstrOp(b,cstrOp(u))));
	inlineIncRef(r = relax(high(u),z,x,y,b));
	inlineIncRef(h = and(l,r)); inlineDecRef(l); inlineDecRef(r);
	inlineIncRef(l = relax(low(u),z,x,y,b));
      }
      else {
	inlineIncRef(h = mkNorm(x,pos(u),addCstrOp(b,negCstrOp(cstrOp(u)))));
	inlineIncRef(r = relax(low(u),z,x,y,b));
	inlineIncRef(l = and(h,r)); inlineDecRef(h); inlineDecRef(r);
	inlineIncRef(h = relax(high(u),z,x,y,b));
      }
    }
    else if (pos(u) == z) {
      if (z==x) {
	inlineIncRef(h = mkNorm(neg(u),y,addCstrOp(b,negCstrOp(cstrOp(u)))));
	inlineIncRef(r = relax(low(u),z,x,y,b));
	inlineIncRef(l = and(h,r)); inlineDecRef(h); inlineDecRef(r);
	inlineIncRef(h = relax(high(u),z,x,y,b));
      }
      else {
	inlineIncRef(l = mkNorm(x,neg(u),addCstrOp(b,cstrOp(u))));
	inlineIncRef(r = relax(high(u),z,x,y,b));
	inlineIncRef(h = and(l,r)); inlineDecRef(l); inlineDecRef(r);
	inlineIncRef(l = relax(low(u),z,x,y,b));
      }
    }
    else {
      inlineIncRef(h = relax(high(u),z,x,y,b));
      inlineIncRef(l = relax(low(u),z,x,y,b));
    }

    r = ite(vars(u),cstrOp(u),h,l);
    inlineDecRef(h); inlineDecRef(l);

    insertRelaxCache(r,u,z,x,y,b);
    return r;
  }  
}


tDDD exists(tVar x, tDDD u) {
  PRECONDITION(isValidVar(x));
  PRECONDITION(isValidDDD(u));

  if (u<=1) {
    return u;
  }
  else {
    tDDD r,h,l;

    if (containsExistsCache(&r,u,x)) {
      return r;
    }

    if (typeOfVar(x)==BOOLEAN) {
      if (pos(u)==x) {
	inlineIncRef(h = high(u));
	inlineIncRef(l = low(u));
	r = or(h,l);
      }
      else {
	inlineIncRef(h = exists(x, high(u)));
	inlineIncRef(l = exists(x, low(u)));
	r = ite(vars(u),cstrOp(u),h,l);
      }
    }
    else if (pos(u)==x || neg(u)==x) {
      inlineIncRef(r = relax(high(u),x,pos(u),neg(u),cstrOp(u)));
      inlineIncRef(h = exists(x, r)); inlineDecRef(r);
      inlineIncRef(r = relax(low(u),x,neg(u),pos(u),negCstrOp(cstrOp(u))));
      inlineIncRef(l = exists(x, r)); inlineDecRef(r);
      r = or(h,l);
    }
    else {
      inlineIncRef(h = exists(x, high(u)));
      inlineIncRef(l = exists(x, low(u)));
      r = ite(vars(u),cstrOp(u),h,l);
    }
    inlineDecRef(h); inlineDecRef(l);
    insertExistsCache(r,u,x);
    return r;
  }
}

tDDD forall(tVar x, tDDD u) {
  tDDD r, tmp1, tmp2;
  PRECONDITION(isValidVar(x) && isValidRefDDD(u));

  inlineIncRef(tmp1 = not(u));
  inlineIncRef(tmp2 = exists(x,tmp1));
  decRef(tmp1);

  r = not(tmp2);
  decRef(tmp2); 

  return r;
}

/* Returns the index of the ddd where the constraint in all
   difference constraints in u containing the variable x has been
   changed to d - that is: x:=x+d */
tDDD increment_aux(tDDD u, tVar x, tCstr d) {
  PRECONDITION(typeOfVar(x)==REAL);
  if (u<=1) {
    return u;
  }
  else {
    tDDD r, l, h;
    tCstrOp c;

    if (containsIncrementCache(&r,u,x,d))
      return r;

    inlineIncRef(l = increment_aux(low(u),x,d));
    inlineIncRef(h = increment_aux(high(u),x,d));

    c = cstrOp(u);
    if (pos(u)==x) 
      c = addCstrOp(c, mkCstrOpLeq(d));
    else if (neg(u)==x) 
      c = addCstrOp(c, mkCstrOpLeq(-d));
  
    r = mk(vars(u), c, h, l);
    inlineDecRef(l);
    inlineDecRef(h);

    insertIncrementCache(r,u,x,d);
    return r;
  }
}

tDDD increment(tDDD u, tVar x, tCstr d) {
  PRECONDITION(isValidRefDDD(u) && isValidVar(x));
  PRECONDITION(typeOfVar(x)==REAL);
  return d==0 ? u : increment_aux(u,x,d);
}

tDDD assign(tDDD u, tVar x, tVar y, tCstr d) {
  PRECONDITION(isValidRefDDD(u) && isValidVar(x) && 
	       isValidVar(y) && isValidCstr(d));
  PRECONDITION(typeOfVar(x)==REAL);
  PRECONDITION(typeOfVar(y)==REAL);

  if (x==y) {
    return increment(u,x,d);
  }
  else {
    /* (E x.(x'=y+d /\ u))[x/x'] = (x'=y+d /\ E x.(u))[x/x']
       = (x-y=d /\ E x.(u)) */
    tDDD r, tmp1, tmp2;
  
    inlineIncRef(tmp1 = mkDiffCstr(x,y,EQ,d));
    inlineIncRef(tmp2 = exists(x,u));
    r = and(tmp1,tmp2);
    inlineDecRef(tmp1);
    inlineDecRef(tmp2);
    return r;
  }
}

static tDDD assignBoolAux(tDDD u, tVar x, unsigned short int b) {
  tDDD t1, t2, t3, r;

  inlineIncRef(t1 = exists(x,u));
  inlineIncRef(t2 = mkBoolVar(x));

  if (b) {
    r = and(t1,t2);
    inlineDecRef(t1); inlineDecRef(t2);
  }
  else {
    inlineIncRef(t3 = not(t2)); inlineDecRef(t2);
    r = and(t1,t3); inlineDecRef(t1); inlineDecRef(t3);
  }
  return r;
}

tDDD assignBool(tDDD u, tVar x, unsigned short int b) {
  PRECONDITION(isValidRefDDD(u) && isValidVar(x));
  PRECONDITION(b==0 || b==1);
  return assignBoolAux(u,x,b);
}


/* Replace y by x */
tDDD replace(tDDD u, tVar x, tVar y) {
  tDDD r;
  PRECONDITION(isValidDDD(u) && isValidVar(x) && isValidVar(y));
  PRECONDITION(typeOfVar(x)==typeOfVar(y));

  if (u<=1 || x==y) {
    return u;
  }
  else if (containsReplaceCache(&r,u,x,y)) {
    return r;
  }
  else if (typeOfVar(x)==BOOLEAN) {
    tDDD t1, t2;
    /* return E y (u /\ x<->y) */
    inlineIncRef(t1 = mkBoolVar(x));
    inlineIncRef(t2 = mkBoolVar(y));
    inlineIncRef(r = biimp(t1,t2)); inlineDecRef(t1); inlineDecRef(t2);
    inlineIncRef(t1 = and(u,r)); inlineDecRef(r);
    r = exists(y,t1); inlineDecRef(t1);
    insertReplaceCache(r,u,x,y);
    return r;
  }
  else {
    tDDD l,h,uh,ul,tmp1,tmp2;
    tVars v;
    tVar p,n;
    tCstrOp c;

    v = vars(u);
    n = negOfVars(v);
    p = posOfVars(v);
    c = cstrOp(u);

    if (p==y) {
      /* replace p-n<~c where p=y <=> x-n<~c  */
      if (n==x) {
	r = replace(mkCstrOpLe(0)<c ? high(u) : low(u),x,y);
	insertReplaceCache(r,u,x,y);
	return r;
      }
      else if (x>n) {
	/* x-n <c -> 1,0 */
	inlineIncRef(uh = mk(mkVars(x,n),c,1,0));
	inlineIncRef(ul = mk(mkVars(x,n),c,0,1));
      }
      else {
	/* x-n <c -> 1,0 <=> n-x -<c */
	inlineIncRef(uh = mk(mkVars(n,x),negCstrOp(c),0,1));
	inlineIncRef(ul = mk(mkVars(n,x),negCstrOp(c),1,0));
      }
    }
    else if (n==y) {
      /* replace p-n<c (n=y) <=> p-x<c = p-x<c */
      if (p==x) {
	/* p-x <c+d -> 1,0 <=> -d<c */
	r = replace(mkCstrOpLe(0)<c ? high(u) : low(u),x,y);
	insertReplaceCache(r,u,x,y);
	return r;
      }
      else if (p>x) {
	/* p-x <c+d -> 1,0 */
	inlineIncRef(uh = mk(mkVars(p,x),c,1,0));
	inlineIncRef(ul = mk(mkVars(p,x),c,0,1));
      }
      else {
	/* p-x <c+d -> 1,0 <=> x-p -(<c+d) */
	inlineIncRef(uh = mk(mkVars(x,p),negCstrOp(c),0,1));
	inlineIncRef(ul = mk(mkVars(x,p),negCstrOp(c),1,0));
      }
    }
    else {
      inlineIncRef(uh = mk(vars(u),c,1,0));
      inlineIncRef(ul = mk(vars(u),c,0,1));
    }
    
    inlineIncRef(h = replace(high(u),x,y));
    inlineIncRef(tmp1 = and(uh,h)); inlineDecRef(uh); inlineDecRef(h);
    inlineIncRef(l = replace(low(u),x,y));
    inlineIncRef(tmp2 = and(ul,l)); inlineDecRef(ul); inlineDecRef(l);
    r = or(tmp1,tmp2); inlineDecRef(tmp1); inlineDecRef(tmp2);

    insertReplaceCache(r,u,x,y);
    return r;
  }
}

bool tautology(tDDD u) {
  PRECONDITION(isValidRefDDD(u));
  resetCstrSystem();
  return allInfeasible(u,0);
}
  
bool satisfiable(tDDD u) {
  PRECONDITION(isValidRefDDD(u));
  resetCstrSystem();
  return existsFeasible(u,1);
}

bool unsatisfiable(tDDD u) {
  PRECONDITION(isValidRefDDD(u));
  resetCstrSystem();
  return allInfeasible(u,1);
}
  
bool falsifiable(tDDD u) {
  PRECONDITION(isValidRefDDD(u));
  resetCstrSystem();
  return existsFeasible(u,0);
}

bool equivalent(tDDD u, tDDD v) {
  PRECONDITION(isValidRefDDD(u) && isValidRefDDD(v));
  resetCstrSystem();
  /* allInfeasible is gc-safe */
  return allInfeasible(biimp(u,v),0);
}

bool consequence(tDDD u, tDDD v) {
  PRECONDITION(isValidRefDDD(u) && isValidRefDDD(v));
  resetCstrSystem();
  /* allInfeasible is gc-safe */
  return allInfeasible(imp(u,v),0);
}

static bool pathReduceAux(tDDD u, tDDD v, tDDD* w, bool boolean_prefix) {
  /* The parameter boolean_prefix indicates whether the path from u to
     v consists entirely of boolean nodes in which case we can use
     dynamic programming.  If, on a path, a real node is encountered,
     this parameter is set to false. */

  if (boolean_prefix && v<=1) {
    *w = v;
    return true;
  }
  else if (boolean_prefix && containsPathReduceCache(w,u)) {
    return true;
  }
  else if (boolean_prefix && typeOfVar(pos(v))==BOOLEAN) {
    bool l_ok, h_ok;
    tDDD l,h;
    assert(u==v);
    l_ok = pathReduceAux(low(v), low(v), &l, true); inlineIncRef(l);
    h_ok = pathReduceAux(high(v), high(v), &h, true); inlineIncRef(h);
    assert(l_ok || h_ok);
    *w = l_ok ? (h_ok ? mk(vars(v),cstrOp(v),h,l) : l) : h;
    inlineDecRef(l); inlineDecRef(h);
    insertPathReduceCache(*w,v);
    return true;
  }
  else {
    /* Check that the path u->v (v exclusive) */
    bool ok = isFeasibleCstrSystem(u,v);

    /* Set w to v so that if u->v is feasible and v<=1 then w has the
       correct value, and if u->v is not feasible then we return
       something that can safely be incRef'ed even though u is not
       path reduced. */
    *w = v;


    /* If the path is feasible and v is not a terminal */
    if (ok && v>1) {
      bool l_ok, h_ok;
      tDDD l,h;

      /* Make a working copy of the current distance array */
      pushCstrSystem();
      addVarsCstrSystem(vars(v));
      assert(path(v)==0);
      l_ok = pathReduceAux(u, low(v), &l, false);
      inlineIncRef(l);
      
      /* Restore the working copy */
      popCstrSystem();
      
      setPath(v);
      h_ok = pathReduceAux(u, high(v), &h, false);
      inlineIncRef(h);
      resetPath(v);
      
      removeVarsCstrSystem(vars(v));

      /* If the path u->v is feasible, then either u->low(v) or
	 u->high(v) (or both) must also be feasible */
      assert(l_ok || h_ok);
      
      *w = l_ok ? (h_ok ? mk(vars(v),cstrOp(v),h,l) : l) : h;
      inlineDecRef(l);
      inlineDecRef(h);
      if (boolean_prefix) {
	insertPathReduceCache(*w,v);
      }
    }
    return ok;
  }
}


tDDD pathReduce(tDDD u) {
  tDDD w;
  bool ok;
  PRECONDITION(isValidRefDDD(u));
  resetCstrSystem();
  ok = pathReduceAux(u, u, &w, true);
  assert(ok);
  return w;
}


static bool isPathReducedAux(tDDD u, tDDD v) {
  /* Check that the path u->v (v exclusive) */
  bool ok = isFeasibleCstrSystem(u,v);

  /* If the path is feasible and v is not a terminal */
  if (ok && v>1) {
    pushCstrSystem();
    addVarsCstrSystem(vars(v));
    assert(path(v)==0);
    ok = isPathReducedAux(u, low(v));
    popCstrSystem();
    if (ok) {
      setPath(v);
      ok = isPathReducedAux(u, high(v));
      resetPath(v);
    }
    removeVarsCstrSystem(vars(v));
  }
  return ok;
}


bool isPathReduced(tDDD u) {
  PRECONDITION(isValidRefDDD(u));
  resetCstrSystem();
  return isPathReducedAux(u, u);
}


tDDD merge(tDDD v) {
  tDDD r;
  if (v<=1) {
    return v;
  }
  else if (containsMergeCache(&r,v)) {
    return r;
  }
  else if (typeOfVar(pos(v))==BOOLEAN) {
    tDDD l,h;
    inlineIncRef(l = merge(low(v)));
    inlineIncRef(h = merge(high(v)));
    r = mk(vars(v),cstrOp(v),h,l);
    inlineDecRef(l); inlineDecRef(h);
  }
  else  {
    unsigned int nc;
    double pc;
    r = v;
    inlineIncRef(v);
    nc = nodeCount(v); pc = pathCount(v);
    if (pc>1 && pc<1000) {
      tDDD s;
      inlineIncRef(s = hull(v));
      if (equivalent(v,s)) {
	r = s;
      }
      inlineDecRef(s);
    }
    inlineDecRef(v);
  }
  insertMergeCache(r,v);
  return r;
}


static void sat_aux(tDDD u, tDDD v, tDDD* res, tMatrix* p) {
  PRECONDITION(isValidDDD(v));

  if (v==0) {
  }
  else if (v==1) {
    pathToMatrix(p, u, v);
    if (apspMatrix(p)) {
      tDDD r,s;
      inlineIncRef(r = matrixToDDD(p));
      inlineIncRef(s = or(r,*res));
      inlineDecRef(r); inlineDecRef(*res);
      *res = s;
    }
  }
  else {
    assert(path(v)==0);
    sat_aux(u, low(v), res, p);
    setPath(v);
    sat_aux(u, high(v), res, p);
    resetPath(v);
  }
}


tDDD saturate(tDDD u) {
  tMatrix* p;
  tDDD res;
  PRECONDITION(isValidRefDDD(u));
  p = allocMatrix(m_curVar);
  res = 0;
  //  clearMatrix(p,negCstrOp(oo));
  sat_aux(u,u,&res,p);
  freeMatrix(p);
  return res;
}


/* Compute the convex hull of the onepath from u to v and add it to
   m_hull */
static void hull_aux(tDDD u, tDDD v, tMatrix* p, tMatrix* h, bool* is_sat) {
  PRECONDITION(isValidDDD(v));

  if (v==0) {
  }
  else if (v==1) {
    /* Follow the path from u to 1 and insert the constraints in p */
    pathToMatrix(p, u, 1);
    
    /* Tighten all constraints in p.  If p is feasible, find
       the convex hull of p and h and store it in h */
    if (apspMatrix(p)) {
      maxMatrix(h, h, p);
      *is_sat = true;
    }
  }
  else {
    assert(path(v)==0);
    hull_aux(u, low(v), p, h, is_sat);
    setPath(v);
    hull_aux(u, high(v), p, h, is_sat);
    resetPath(v);
  }
}


/* hull(u) prints the convex hull of u */
tDDD hull(tDDD u) {
  tDDD res;
  tMatrix *p, *h;
  bool is_sat;
  PRECONDITION(isValidRefDDD(u));

  /* Mark the current hull as invalid */
  is_sat = false;

  /* Allocate the two matrices */
  p = allocMatrix(m_curVar);
  h = allocMatrix(m_curVar);

  /* Set the current result matrix to the ff matrix */
  clearMatrix(h,negCstrOp(oo));
  
  /* Update the resulting matrix */
  hull_aux(u,u,p,h,&is_sat);
  res = is_sat ? matrixToDDD(h) : 0;

  /* Free the two matrices */
  freeMatrix(h);
  freeMatrix(p);
  return res;
}

void anysat(tDDD u) {
  PRECONDITION(isValidRefDDD(u));

  resetCstrSystem();
  if (hasFeasibleSolution(u,1))
    printFeasibleSolution();
  else
    printMsg("False has no satisfying variable assignment.\n");
}

void anynonsat(tDDD u) {
  PRECONDITION(isValidRefDDD(u));

  resetCstrSystem();
  if (hasFeasibleSolution(u,0))
    printFeasibleSolution();
  else
    printMsg("True has no falsifying variable assignment.\n");
}

unsigned int nodeCountAux(tDDD u) {
  if (u<=1 || mark(u)==1) {
    return 0;
  }
  else {
    setMark(u);
    return 1 + nodeCountAux(low(u)) + nodeCountAux(high(u));
  }
}

unsigned int nodeCount(tDDD u) {
  unsigned int r;

  PRECONDITION(isValidRefDDD(u));

  if (u<=1) {
    r = 1;
  }
  else {
    r = nodeCountAux(u)+2;
    unmarkNode(u);
  }

  return r;
}

double pathCount(tDDD u) {
  double r = 0.0;
  
  PRECONDITION(isValidDDD(u));
  if (u==0) {
    r = 0.0;
  }
  else if (u==1) {
    r = 1.0;
  }
  else if (!containsPathCountCache(&r,u)) {
    r = pathCount(low(u)) + pathCount(high(u));
    insertPathCountCache(r,u);
  }

  return r;
}


unsigned int varCountAux(tDDD u, char* v) {
  if (u<=1 || mark(u)==1) {
    return 0;
  }
  else {
    unsigned int r = 0;
    setMark(u);
    if (v[pos(u)]==0) {
        v[pos(u)]++;
	r++;
    }
    if (v[neg(u)]==0) {
        v[neg(u)]++;
	r++;
    }
    return r + varCountAux(low(u),v) + varCountAux(high(u),v);
  }
}

unsigned int varCount(tDDD u) {
  PRECONDITION(isValidDDD(u));

  if (u<=1) {
    return 0;
  }
  else {
    unsigned int r;
    char* v = allocMem(v, m_curVar * sizeof(char));
    setMem(v, 0, m_curVar * sizeof(char));
    r = varCountAux(u,v);
    unmarkNode(u);
    freeMem(v);
    return r;
  }
}

bool isFreeVarAux(tVar x, tDDD u) {
  if (u<=1 || mark(u)==1) {
    return true;
  }
  else {
     setMark(u);
     return (x!=pos(u) && x!=neg(u))
      && isFreeVarAux(x, high(u))
      && isFreeVarAux(x, low(u));
  }
}

bool isFreeVar(tVar x, tDDD u) {
  bool r;
  PRECONDITION(isValidRefDDD(u));
  PRECONDITION(isValidVar(x));
  r = isFreeVarAux(x,u);
  unmarkNode(u);
  return r;
}

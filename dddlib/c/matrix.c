#include <stdio.h>
#include <stdlib.h>
#include "table.h"
#include "matrix.h"
#include "io.h"
#include "variable.h"
#include "functions.h"

/* Construct a new matrix */
tMatrix* allocMatrix(unsigned int dim) {
  tMatrix* M = allocMem(M, sizeof(tMatrix));
  M->m_data = allocMem(M->m_data, sizeof(tCstrOp) * dim * dim);
  M->m_used = allocMem(M->m_used, sizeof(bool) * dim);
  setMem(M->m_used, 0, sizeof(bool) * dim);
  M->m_dim = dim;
  return M;
}

/* Delete a matrix */
void freeMatrix(tMatrix* M) {
  M->m_dim = -1;
  freeMem(M->m_data);
  freeMem(M->m_used);
  freeMem(M);
}
 
void copyMatrix(tMatrix* dest, const tMatrix* src) {
  PRECONDITION(dest->m_dim == src->m_dim);
  copyMem(dest->m_data, src->m_data, sizeof(tCstrOp) * dest->m_dim * dest->m_dim);
  copyMem(dest->m_used, src->m_used, sizeof(bool) * dest->m_dim);
}

void clearMatrix(tMatrix* dest, tCstrOp v) {
  unsigned int i;
  for (i=0; i<dest->m_dim*dest->m_dim; i++)
    dest->m_data[i] = v;
  setMem(dest->m_used, 0, sizeof(bool) * dest->m_dim);
}

static __inline__ tCstrOp max(tCstrOp l, tCstrOp r) {
  return l > r ? l : r;
}

void maxMatrix(tMatrix* res, const tMatrix* l, const tMatrix* r) {
  unsigned int i;
  PRECONDITION(res->m_dim==l->m_dim && l->m_dim==r->m_dim);

  for (i=0; i<res->m_dim*res->m_dim; i++)
    res->m_data[i] = max(l->m_data[i], r->m_data[i]);
  for (i=0; i<res->m_dim; i++)
    res->m_used[i] = l->m_used[i] || r->m_used[i];
}


/* Print the matrix */
void printMatrix(const tMatrix* M) {
  tVar p, n;

  for (p=1; p<m_curVar; p++) {
    for (n=0; n<p; n++) {
      
      if (typeOfVar(p)==REAL && typeOfVar(n)==REAL) {

	/* n - p c <=> p - n neg(c) */
	tCstrOp c = lookupMatrix(M,p,n);

	/* p - n d */ 
	tCstrOp d = lookupMatrix(M,n,p);

	/* Check whether any constraints should be printed */
	if (c!=oo || d!=oo) {

	  /* Print the positive and negative variable */
	  printVars(mkVars(p,n));
	
	  /* Check whether p-n is an exact value */
	  if (valueOfCstrOp(negCstrOp(c))==valueOfCstrOp(d) &&
	      opOfCstrOp(c)==opOfCstrOp(d)) {
	    printMsg(" is ");

	    /* Print the constraint. */
	    printCstr(valueOfCstrOp(d));
	  }
	  else {
	    printMsg(" in ");
	    /* Print the negated constraint. */
	    if (c==oo)
	      printMsg("]-oo");
	    else {
	      printMsg("%c", opOfCstrOp(c)==COMP_LESS ? ']' : '[');
	      printCstr(valueOfCstrOp(negCstrOp(c)));
	    }
	    printMsg("; ");
	    /* Print the positive constraint. */
	    if (d==oo)
	      printMsg("oo[");
	    else {
	      printCstr(valueOfCstrOp(d));
	      printMsg("%c", opOfCstrOp(d)==COMP_LESS ? '[' : ']');
	    }
	  }
	  printMsg("\n");
	}
      }
    }
  }
}


tDDD matrixToDDD(const tMatrix* M) {
  tDDD r = 1;
  int l, k;

  for (l=0; l<M->m_dim; l++)
    if (lookupMatrix(M,l,l)<mkCstrOpLeq(0))
      return 0;
  {
    const tVars* const variables = getOrder(&k);
    for (l = k-1; l>=0; l--) {
      tVar i, j;
      tCstrOp Mij,Mji;
      j = posOfVars(variables[l]);
      i = negOfVars(variables[l]);
      PRECONDITION(j>i);
      Mij = lookupMatrix(M,i,j);
      Mji = lookupMatrix(M,j,i);
      if (negCstrOp(Mji)==Mij)
	return 0;
      if (Mij != oo) {
	inlineIncRef(r = mk(mkVars(j,i), Mij, r,0));
	inlineDecRef(high(r));
      }
      if (Mji != oo) {
	inlineIncRef(r = mk(mkVars(j,i), negCstrOp(Mji), 0,r));
	inlineDecRef(low(r));
      }
    }
    inlineDecRef(r);
  }
  return r;
}

void pathToMatrix(tMatrix* M, tDDD u, tDDD v) {
  clearMatrix(M,oo);
  while (u!=v) {
    assert(u!=0);
    if (path(u)==0) {
      insertMatrix(M,pos(u),neg(u),negCstrOp(cstrOp(u)));
      u = low(u);
    }
    else {
      insertMatrix(M,neg(u),pos(u),cstrOp(u));
      u = high(u);
    }
  }
}

void projectMatrix(tMatrix* M, tVar i) {
  tVar k;
  for (k=0; k<M->m_dim; k++) {
    insertMatrix(M,i,k,oo);
    insertMatrix(M,k,i,oo);
  }
  M->m_used[i] = false;
}


bool apspMatrix(tMatrix* M) {
  unsigned int i,j,k;
  
  /* Run Floyd-Warshall and check for negative cycles */
  for (k=0; k<M->m_dim; k++) {
    if (M->m_used[k] && typeOfVar(k)==REAL) {
      for (i=0; i<M->m_dim; i++) {
	if (M->m_used[i] && typeOfVar(i)==REAL) {
	  for (j=0; j<M->m_dim; j++) {
	    if (M->m_used[j] && typeOfVar(j)==REAL) {
	      /* Use i->k->j if it is shorter than i->j. */
	      if (lookupMatrix(M, i,k)!=oo && lookupMatrix(M, k,j)!=oo) {
		tCstrOp ikj = addCstrOp(lookupMatrix(M, i,k),
					lookupMatrix(M, k,j));
		if (ikj<lookupMatrix(M, i,j)) {
		  insertMatrix(M,i,j,ikj);
		  
		  /* Stop if we insert a negative cycle */
		  if (i==j && lookupMatrix(M,i,i) < mkCstrOpLeq(0)) {
		    insertMatrix(M,0,0, negCstrOp(oo));
		    return false;
		  }
		}
	      }
	    }
	  }
	}
      }
    }
  }
  return true;
}

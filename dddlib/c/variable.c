#include <string.h>
#include "variable.h"
#include "memory.h"
#include "io.h"
#include "init.h"
#include "table.h"

/* The variable module defines functions for constructing new
   difference constraint variables.  The function mkVar(const char*)
   returns a unique identifier for a difference constraint variable
   (of type tVar) given the name of the difference constraint
   variable. The name is used only for pretty printing purposes.

   Difference constraint variables (tVar) are simply called
   'variables'.  Two variables (a positive and negative) can be
   combined into a variable pair (of type tVars) using the function
   mkVars(tVar p, tVar n).  The negative variable is stored in the 16
   most significant bits, and the positive variable is stored in the
   16 least significant bits.  With this encoding, variable pairs can
   be compared directly using <, >=, etc. yielding the correct result
   with the ordering n<p.

   It is only possible to construct m_maxVar variables.
*/

const tCstrOp oo = (CSTR_MAX<<1) | COMP_LESSEQ;

tVar m_maxVar;
tVar m_curVar;

/* The definition of a variable identifier. */
typedef struct {
  const char* name;
  tVarType type;
  tVarKind kind; 
} tVarId;

/* Variable identifiers. */
static tVarId* m_id;

/* Initial size of the m_name (which holds all identifier strings). */
#define INITIAL_SIZE 2

/* All variable names */
static char* m_name;

/* Current index in m_name */
static unsigned int m_current;

/* The next free index in m_name. */
static unsigned int m_size;

/* Ordering variables */
static int* m_order;
static int m_lastID;
static int m_firstID;
static tVar m_order_size;
static tVars* m_variables;
static int m_length;
static bool m_orderchanged;

void initVariable(void) {
  m_curVar = 0;
  m_size = INITIAL_SIZE;
  m_current = 0;
  m_id = allocMem(m_id, m_maxVar * sizeof(tVarId));
  m_name = allocMem(m_name, INITIAL_SIZE * sizeof(char));
  m_order_size = m_maxVar;
  m_order = allocMem(m_order, (m_order_size+1) * (m_order_size+1) * sizeof(int));
  m_variables = allocMem(m_variables, m_maxVar*m_maxVar*sizeof(tVars));
  clearOrder();
}

void reinitVariable(void) {
  tVar i,j;
  int* neworder;

  m_id = reallocMem(m_id, m_maxVar * sizeof(tVarId));
  neworder = allocMem(neworder, (m_maxVar+1) * (m_maxVar+1) * sizeof(int));
  for (i=0; i<m_order_size; i++)
    for (j=0; j<m_order_size; j++)
      neworder[i*m_maxVar+j] = m_order[i*m_order_size+j];
  m_order_size = m_maxVar;
  freeMem(m_order);
  freeMem(m_variables);

  m_order = neworder;
  m_variables = allocMem(m_variables, m_maxVar*m_maxVar*sizeof(tVars));
  m_order[m_order_size*pos(0) + neg(0)] = INT_MAX;
  m_order[m_order_size*pos(1) + neg(1)] = INT_MAX;
  m_orderchanged = true;

}

void doneVariable(void) {
  freeMem(m_name);
  freeMem(m_id);
  freeMem(m_order);
  freeMem(m_variables);
}

void infoVariable(void) {
  printMsg("%d variables (currently max. %d).\n", m_curVar, m_maxVar);
  printMsg("%d variable pairs:\n", m_lastID-m_firstID);
  printOrder();
}

static __inline__ tVar mkTypedVar(const char* name, tVarType type,
				  tVarKind kind) {
  unsigned int length;
  tVar i,number;
  PRECONDITION(m_name!=NULL);
  PRECONDITION(m_size>=m_current);

  if (m_curVar==m_maxVar) {
    /* Double the number of variables */
    reinit_vars(2*m_maxVar);
  }

  length = strlen(name)+1;
  if (m_size-m_current<length) {
    /* Resize m_name if name does not fit into it */
    char* newname;
    do { m_size *= 2; } while (m_size-m_current<length);
    newname = reallocMem(m_name, m_size);
    if (newname!=m_name) {
      /* Update the pointers in m_id */
      for (i=0; i<m_curVar; i++)
	m_id[i].name += (newname - m_name);
      m_name = newname;
    }
  }
  /* Copy the name to the current position in m_name. */
  strcpy(&m_name[m_current], name);

  /* Set the name, type, kind */
  m_id[m_curVar].name = &m_name[m_current];
  m_id[m_curVar].type = type;
  m_id[m_curVar].kind = kind;
  m_current += length;

  number = m_curVar++;
  if (type==BOOLEAN && kind==USER)
    appendBoolToOrder(number);
  else if (type==REAL) {
    for (i=0; i<m_curVar; i++) {
      if (typeOfVar(i)==REAL)
	appendRealToOrder(number,i);
    }
  }
  return number;
}

tVar mkVar(const char* name) {
  return mkTypedVar(name, REAL, USER);
}

tVar mkBool(const char* name) {
  /* Create the null variable (with no name) for this Boolean variable */
  mkTypedVar("", BOOLEAN, INTERNAL);
  return mkTypedVar(name, BOOLEAN, USER);
}

tVarType typeOfVar(tVar v) {
#ifdef DEBUG
  if (!isValidVar(v)) {
    printf("\nYou have not initialised a variable properly. "
	   "In C++ you should write something like:\n\n"
	   "    boolean b = \"name\";\n    real z = \"z\"\n\n");
    exit(1);
  }
#endif
  return m_id[v].type; 
}

tVarKind kindOfVar(tVar v) {
  assert(isValidVar(v));
  return m_id[v].kind; 
}

const char* nameOfVar(tVar v) {
  assert(isValidVar(v));
  return m_id[v].name; 
}

bool isValidVar(tVar v) {
  return v<m_curVar;
}

bool isValidVars(tVars v) {
  return 
    isValidVar(posOfVars(v)) &&
    isValidVar(negOfVars(v)) &&
    posOfVars(v) > negOfVars(v);
}


int orderOfPair(tVar x, tVar y) {
  PRECONDITION(isValidVar(x) || x==m_maxVar);
  PRECONDITION(isValidVar(y) || y==m_maxVar);
  return m_order[x*m_order_size+y];
}

int orderOf(tVars v) {
  return orderOfPair(posOfVars(v), negOfVars(v));
}

static void updateOrder(tVar x, tVar y, int value) {
  PRECONDITION(isValidVar(x));
  PRECONDITION(isValidVar(y));
  m_order[x*m_order_size+y] = m_order[y*m_order_size+x] = value;
  m_orderchanged = true;
}  

void appendRealToOrder(tVar x, tVar y) {
  PRECONDITION(typeOfVar(x)==REAL);
  PRECONDITION(typeOfVar(y)==REAL);
  updateOrder(x,y,++m_lastID);
}

void prependRealToOrder(tVar x, tVar y) {
  PRECONDITION(typeOfVar(x)==REAL);
  PRECONDITION(typeOfVar(y)==REAL);
  updateOrder(x,y,--m_firstID);
}

void appendBoolToOrder(tVar x) {
  PRECONDITION(typeOfVar(x)==BOOLEAN);
  PRECONDITION(kindOfVar(x)==USER && kindOfVar(x-1)==INTERNAL);
  updateOrder(x,x-1,++m_lastID);
}

void prependBoolToOrder(tVar x) {
  PRECONDITION(typeOfVar(x)==BOOLEAN);
  PRECONDITION(kindOfVar(x)==USER && kindOfVar(x-1)==INTERNAL);
  updateOrder(x,x-1,--m_firstID);
}

void appendIncrLexToOrder(void) {
  tVar i,j;
  for (j=0; j<m_curVar-1; j++)
    for (i=j+1; i<m_curVar; i++)
      if ( orderOfPair(i,j)==0 &&
	   ((typeOfVar(i)==REAL && typeOfVar(j)==REAL) ||
	    (typeOfVar(i)==BOOLEAN && kindOfVar(i)==USER && j==i-1)) )
	updateOrder(i,j,++m_lastID);
}

void prependIncrLexToOrder(void) {
  tVar i,j;
  for (i=m_curVar-1; i != 0; i--)
    for (j=i-1; j != (tVar)-1; j--)
      if ( orderOfPair(i,j)==0 &&
	   ((typeOfVar(i)==REAL && typeOfVar(j)==REAL) ||
	    (typeOfVar(i)==BOOLEAN && kindOfVar(i)==USER && j==i-1)) )
	updateOrder(i,j,--m_firstID);
}
void appendDecrLexToOrder(void) {
  tVar i,j;
  for (i=m_curVar-1; i != 0; i--)
    for (j=i-1; j != (tVar)-1; j--)
      if ( orderOfPair(i,j)==0 &&
	   ((typeOfVar(i)==REAL && typeOfVar(j)==REAL) ||
	    (typeOfVar(i)==BOOLEAN && kindOfVar(i)==USER && j==i-1)) )
	updateOrder(i,j,++m_lastID);
}

void prependDecrLexToOrder(void) {
  tVar i,j;
  for (j=0; j<m_curVar-1; j++)
    for (i=j+1; i<m_curVar; i++)
      if ( orderOfPair(i,j)==0 &&
	   ((typeOfVar(i)==REAL && typeOfVar(j)==REAL) ||
	    (typeOfVar(i)==BOOLEAN && kindOfVar(i)==USER && j==i-1)) )
	updateOrder(i,j,--m_firstID);
}


void clearOrder(void) {
  setMem(m_order, 0, (m_order_size+1)*(m_order_size+1)*sizeof(int));
  m_firstID = 0;
  m_lastID = 0;
  m_order[m_order_size*pos(0) + neg(0)] = INT_MAX;
  m_order[m_order_size*pos(1) + neg(1)] = INT_MAX;
  m_orderchanged = true;
}

int cmpVars(const tVars* a, const tVars* b) {
  return orderOf(*a)<orderOf(*b) ? -1 
       : orderOf(*a)>orderOf(*b) ? 1
       : 0;
}

const tVars* const getOrder(int* length) {
  tVar i,j;
  typedef int (*cmpFunc)(const void*, const void*);
  if (m_orderchanged) {
    for (m_length=0,j=0; j<m_curVar-1; j++)
      for (i=j+1; i<m_curVar; i++)
	if ( orderOfPair(i,j)!=0  &&
	     ((typeOfVar(i)==REAL && typeOfVar(j)==REAL) ||
	      (typeOfVar(i)==BOOLEAN && kindOfVar(i)==USER && j==i-1)) )
	  m_variables[m_length++] = mkVars(i,j);
    qsort(m_variables, m_length, sizeof(tVars), (cmpFunc)cmpVars);
    m_orderchanged = false;
  }
  *length = m_length;
  return m_variables;
}


void printOrder(void) {
  tVar i,j,k;
  int l;
  const tVars* const variables = getOrder(&l);
  printMsg("Ordering is:\n");

  for (k=0; k<l; k++) {
    i = posOfVars(variables[k]);
    j = negOfVars(variables[k]);
    if (typeOfVar(i)==REAL && typeOfVar(j)==REAL && i>j) {
      printMsg("  ddd::AppendToOrder(%s,%s);\n",nameOfVar(i), nameOfVar(j));
    }
    else if (typeOfVar(i)==BOOLEAN && kindOfVar(i)==USER && j==i-1) {
      printMsg("  ddd::AppendToOrder(%s);\n",nameOfVar(i));
    }
  }	  
  printMsg("\n");
}



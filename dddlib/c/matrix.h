#ifndef MATRIX_H
#define MATRIX_H

#include "types.h"
#include "memory.h"

/* The matrix elements are stored in an array in a row-major manner,
   that is an array with m_dim * m_dim elements, where the first m_dim
   elements are row 0, the next m_dim elements are row 1, etc. */

typedef struct {
  tCstrOp* m_data;
  unsigned int m_dim;
  // m_used[v] is true iff variable v is a node in the graph 
  bool* m_used;
} tMatrix;


/* Construct a new matrix */
tMatrix* allocMatrix(unsigned int dim);

/* Delete a matrix */
void freeMatrix(tMatrix* M);

/* Copy a matrix from Msrc to Mdest */
void copyMatrix(tMatrix* dest, const tMatrix* src);

/* Set all elements to the value val */
void clearMatrix(tMatrix* M, tCstrOp val);

/* Insertion */
static __inline__ void 
insertMatrix(tMatrix*, tVar row, tVar col, tCstrOp val) ATTR_UNUSED;

/* Lookup */
static __inline__ tCstrOp
lookupMatrix(const tMatrix*, tVar row, tVar col) ATTR_UNUSED;

/* For each element in l and r take the largest and put it in res */
void maxMatrix(tMatrix* res, const tMatrix* l, const tMatrix* r);

/* Print matrix */
void printMatrix(const tMatrix* M);

/* Given a feasible tight matrix M, return the 1-path represented by M  */
tDDD matrixToDDD(const tMatrix* M);

/* Insert the constraints along the path u to v. */
void pathToMatrix(tMatrix* M, tDDD u, tDDD v);

/* Project the matrix onto a variable */
void projectMatrix(tMatrix* M, tVar i);

/* apspMatrix(M) uses the Floyd-Warshall all-pair-shortest path algorithm
   if M is feasible the algorithm returns true and M has been tightened.
   if M is infeasible the algorithm returns false and M is undefined */
bool apspMatrix(tMatrix* M);


/* Inline definitions ------------------------------------------------------ */

static __inline__ void
insertMatrix(tMatrix* M, tVar row, tVar col, tCstrOp val) {
  M->m_used[row] = M->m_used[col] = true;
  PRECONDITION(row < M->m_dim && col < M->m_dim);
  M->m_data[M->m_dim*row + col] = val;
}

static __inline__ tCstrOp
lookupMatrix(const tMatrix* M, tVar row, tVar col) {
  PRECONDITION(row < M->m_dim && col < M->m_dim);
  return M->m_data[M->m_dim*row + col];
}

#endif

#include "primes.h"
#include "table.h"
#include "memory.h"
#include "io.h"
#include "cache.h"

typedef struct {
#ifdef DEBUG
  unsigned int calls;
  unsigned int elements;
  unsigned int comparisons;
  unsigned int externalReferences;
#endif
  unsigned int cleared;
  unsigned int recycled;
} tTableInfo;

static tTableInfo m_tableInfo;

/* The ddd table */
tDDDNode* m_ddd;


/* Copy of m_tableSize (only used when resizing) */
static unsigned int m_currentTableSize;

/* Freelist */
static tDDD m_freelist;

/* Function to be called before a garbage collection */
static tPreGC m_preGC = NULL;

void set_pre_gc(tPreGC f) {
  m_preGC = f;
}

void initTable(void) {
  tDDD i;
  tDDDNode* node;

  /* Calculate the first primes below or equal to the user's size hints */
  m_tableSize = prevPrime(ceil(m_mem_size/sizeof(tDDDNode)));


#ifdef DEBUG
  m_tableInfo.calls = m_tableInfo.elements = 0;
  m_tableInfo.comparisons = m_tableInfo.externalReferences = 0;
#endif
  m_tableInfo.cleared = m_tableInfo.recycled = 0;
  
  /* Allocate the ddd table */
  m_ddd = allocMem(m_ddd, m_tableSize * sizeof(tDDDNode));

  m_currentTableSize = m_tableSize;

  /* Initialize the table */
  for (i=0; i<m_tableSize; i++) {
    node = &m_ddd[i];
#ifdef DEBUG
    /* Make sure the unused nodes contain detectable garbage */
    node->vars = UINT_MAX;
    node->cstrOp = oo;
    node->low = UINT_MAX;
    node->high = UINT_MAX;
#endif
    node->next = i+1;
    node->hash = 0;
    node->ref  = 0;
    node->mark = 0;
    node->path = 0;
  }

  /* The last entry in the free list should point to "bottom" */
  m_ddd[m_tableSize-1].next = 0;

  /* The variables at index 0 and 1 should have the largest value
     This is used intensively when ordering/comparing nodes */
  m_ddd[0].vars = m_ddd[1].vars = (m_maxVar<<16)|m_maxVar;

  /* The variables at index 0 and 1 should not have references counted.
     To avoid assertions to fail the reference field is initialized to max */
  m_ddd[0].ref = m_ddd[1].ref = MAX_REF;

  /* The ddd's for true and false are at position 0 and 1 in the
     table, so the first free entry is at 2 */
  m_freelist = 2;
}

void reinitTable(void) {
  m_ddd[0].vars = m_ddd[1].vars = (m_maxVar<<16)|m_maxVar;
}

void doneTable(void) {
  freeMem(m_ddd);
}

void infoTable(void) {
  printMsg("DDD table: %u entries (%u kb)",
	   m_tableSize, (m_tableSize*sizeof(tDDDNode))/1024);
  if (m_tableInfo.cleared>0)
    printMsg("%u garbage collections, %u nodes recycled\n",
	     m_tableInfo.cleared, m_tableInfo.recycled);
#ifdef DEBUG
  printMsg(" with %u elements, %u insertions, %u hash-chain comp., and %u references",
	   m_tableInfo.elements, m_tableInfo.calls, m_tableInfo.comparisons, 
	   m_tableInfo.externalReferences);
#endif
  printMsg("\n");
}

static __inline__ void gc(void) {
  tDDD i;
  tDDDNode* node;
  unsigned int recycled = 0;

  printMsg("Garbage collection: "); fflush(stdout);

  /* Clear all caches */
  clearCache();
  if (m_preGC != NULL)
    (*m_preGC)();

  for (i=m_tableSize-1; i>=2; i--) {
    /* Mark all nodes that are referenced */
    if (m_ddd[i].ref>0 && m_ddd[i].mark==0)
      markNode(i);
  }

  for (i=0; i<m_tableSize; i++) {
    /* Reset all entries in the hashtable to zero */
    m_ddd[i].hash = 0;
  }

  /* Put all unreachable nodes on the freelist  */
  m_freelist = 0;
  for (i=m_tableSize-1; i>=2; i--) {
    node = &m_ddd[i];
    if (node->mark==0) {
      /* Unreachable node */
      recycled++;
#ifdef DEBUG
      node->vars = UINT_MAX;
      node->cstrOp = oo;
      node->low = UINT_MAX;
      node->high = UINT_MAX;
#endif      
      node->next = m_freelist;
      m_freelist = i;
    }
    else {
      tDDD hv;
      /* Remove the mark and reinsert the node in the hash table */
      resetMark(i);
      hv = HASH4(node->vars, node->cstrOp, node->low, node->high)%m_tableSize;
      node->next = m_ddd[hv].hash;
      m_ddd[hv].hash = i;
    }
  }

  m_tableInfo.cleared++;
  m_tableInfo.recycled += recycled;

  printMsg("%u nodes free.\n", recycled);

}



tDDD insertTable(tVars v, tCstrOp c, tDDD l, tDDD h) {
  tDDD u,hv;
  tDDDNode* node;

  assert(l<m_tableSize);
  assert(h<m_tableSize);

#ifdef DEBUG
  m_tableInfo.calls++;
#endif

  hv = HASH4(v,c,l,h)%m_tableSize;
  u = m_ddd[hv].hash;

  /* Try to find the node in the hash table chain */
  while (u!=0) {
#ifdef DEBUG
    m_tableInfo.comparisons++;
#endif
    node = &(m_ddd[u]);
    if (node->vars==v && node->cstrOp==c && node->low==l && node->high==h) {
      return u;
    }
    u = node->next;
  }
  
  /* If there is no more memory call the garbage collector */
  if (m_freelist==0) {
    gc();
    /* If that did not help, increase the table size */
    if (m_freelist==0) {
      m_errorHandler(MSG_NO_DDD_ENTRIES, m_tableSize);
    }
  }

  /* Get the next free position in the table */
  u = m_freelist;

  /* Insert the data in the table */
  node = &m_ddd[u];
  node->vars = v;
  node->cstrOp = c;
  node->low = l;
  node->high = h;
  
  /* Update the freelist and make u the new head in the hash chain */
  m_freelist = node->next;
  node->next = m_ddd[hv].hash;
  m_ddd[hv].hash = u;

#ifdef DEBUG
  m_tableInfo.elements++;
#endif

  return u;
}


void incRef(tDDD u) {
#ifdef DEBUG
  if (u>1)
    m_tableInfo.externalReferences++;
#endif
  inlineIncRef(u);
}

void decRef(tDDD u) {
#ifdef DEBUG
  if (u>1) {
    //    assert(m_tableInfo.externalReferences>0);
    m_tableInfo.externalReferences--;
  }
#endif
  inlineDecRef(u);
}

bool isTerm(tDDD u) {
  return u <= 1;
}

bool isNonTerm(tDDD u) {
  return !isTerm(u); 
}

bool isValidDDD(tDDD u) {
  bool posValid = isValidVar(posOfVars(m_ddd[u].vars));
  bool negValid = isValidVar(negOfVars(m_ddd[u].vars));
  bool cstrValid = m_ddd[u].cstrOp != oo;
  bool nodeValid = u<m_tableSize;
  bool lowValid = m_ddd[u].low<m_tableSize;
  bool highValid = m_ddd[u].high<m_tableSize;
  return isTerm(u) || 
    (posValid && negValid && cstrValid &&
     nodeValid && lowValid && highValid);
}

bool isValidRefDDD(tDDD u) {
  return isValidDDD(u) && ref(u)>0;
}

#include "init.h"
#include "table.h"
#include "cache.h"
#include "primes.h"
#include "constraint-system.h"
#include "variable.h"
#include "io.h"


void init(tVar maxVar, double mb_mem_usage, double mem_ratio) {
  if (m_isInitialized) {
    printf("Error: init() called twice.\n");
    exit(1);
  }
  if (mb_mem_usage<=0)  {
    printf("Error: init() must be called with mb_mem_usage>0.\n");
    exit(1);
  }
  if (mem_ratio<=0 || mem_ratio>=1)  {
    printf("Error: init() must be called with 0<mem_ratio<1.\n");
    exit(1);
  }

  m_mem_size = ceil(1024*1024*mb_mem_usage*mem_ratio);
  m_cache_size = ceil(1024*1024*mb_mem_usage*(1-mem_ratio));

  m_isInitialized = true;
  m_maxVar = maxVar;

  /* The memory module must be initialized before all other modules */
  initMem();

  initPrimes();
  initTable();
  initVariable();
  initCache();
  initCstrSystem();
  initIo();
}

void reinit_vars(tVar maxVar) {
#ifdef DEBUG
  printf("Changing #variables from %d to %d.\n",m_maxVar,maxVar);
#endif
  m_maxVar = maxVar;

  reinitTable();
  reinitVariable();
  reinitCstrSystem();
}

void done(void) {
  assert(m_isInitialized);
  m_isInitialized = false;

  donePrimes();
  doneTable();
  doneVariable();
  doneCache();
  doneCstrSystem();
  doneIo();

  /* The memory module must be the last module finalized. */
  doneMem();
}

void info(void) {
  printMsg("\n%s\n\n", DDDIDSTRING);
  infoTable();
  infoCache();
  infoMem();
  infoVariable();
}

#ifndef CACHE_H
#define CACHE_H

#include "types.h"
#include "memory.h"
#include "primes.h"
#include "io.h"

#define ApplyCacheRatio      0.5
#define RelaxCacheRatio      0.15
#define ExistsCacheRatio     0.15
#define ReplaceCacheRatio    0.10
#define PathReduceCacheRatio 0.04
#define PathCountCacheRatio  0.02
#define MergeCacheRatio      0.02
#define IncrementCacheRatio  0.01

static __inline__ void initCache(void) ATTR_UNUSED;
static __inline__ void doneCache(void) ATTR_UNUSED;
static __inline__ void infoCache(void) ATTR_UNUSED;
static __inline__ void clearCache(void) ATTR_UNUSED;

static __inline__ void
insertApplyCache(tDDD r, int o, tDDD u, tDDD v) ATTR_UNUSED;
static __inline__ bool
containsApplyCache(tDDD* r, int o, tDDD u, tDDD v) ATTR_UNUSED;

static __inline__ void
insertPathCountCache(double r, tDDD u) ATTR_UNUSED;
static __inline__ bool
containsPathCountCache(double* r, tDDD u) ATTR_UNUSED;

static __inline__ void
insertReplaceCache(tDDD r, tDDD u, tVar x, tVar y) ATTR_UNUSED;
static __inline__ bool
containsReplaceCache(tDDD* r, tDDD u, tVar x, tVar y) ATTR_UNUSED;

static __inline__ void
insertIncrementCache(tDDD r, tDDD u, tVar x, tCstr d) ATTR_UNUSED;
static __inline__ bool
containsIncrementCache(tDDD* r, tDDD u, tVar x, tCstr d) ATTR_UNUSED;

static __inline__ void
insertRelaxCache(tDDD r, tDDD u, tVar z, tVar x, tVar y, tCstrOp b) ATTR_UNUSED;
static __inline__ bool
containsRelaxCache(tDDD* r, tDDD u, tVar z, tVar x, tVar y, tCstrOp b) ATTR_UNUSED;

static __inline__ void
insertExistsCache(tDDD r, tDDD u, tVar x) ATTR_UNUSED;
static __inline__ bool
containsExistsCache(tDDD* r, tDDD u, tVar x) ATTR_UNUSED;

static __inline__ void
insertMergeCache(tDDD r, tDDD u) ATTR_UNUSED;
static __inline__ bool
containsMergeCache(tDDD* r, tDDD u) ATTR_UNUSED;

static __inline__ void
insertPathReduceCache(tDDD r, tDDD u) ATTR_UNUSED;
static __inline__ bool
containsPathReduceCache(tDDD* r, tDDD u) ATTR_UNUSED;

typedef struct {
  void* data;
  char* name;
  unsigned int size;
  unsigned int entry_size;
#ifdef DEBUG
  unsigned int cleared;
  unsigned int inserted;
  unsigned int hits;
  unsigned int misses;
#endif
} tCache;

static __inline__ void cacheInfo(tCache* cache) {
  printMsg("%s cache: %u entries (%u kb)", cache->name,
	   cache->size, (cache->size*cache->entry_size)/1024);
#ifdef DEBUG
  printMsg(" with %u/%u/%u inserted/hits/misses",
	   cache->inserted, cache->hits, cache->misses);
#endif
  printMsg("\n");
}

static __inline__ void cacheClear(tCache* cache) {
  /* Initialize the cache to empty by setting all values to 0. */
  setMem(cache->data, 0, cache->size*cache->entry_size);
#ifdef DEBUG
  cache->cleared++;
#endif
}

#define cacheInit(name) \
m_##name##Cache = cacheInitAux(#name, name##CacheRatio, sizeof(name##CacheNode))

static __inline__ tCache* cacheInitAux(const char* name, double ratio, unsigned int entry_size) {
  tCache* cache;
  cache = allocMem(cache, sizeof(tCache));
  cache->size = prevPrime(ceil(ratio*m_cache_size)/entry_size);
  cache->entry_size = entry_size;
#ifdef DEBUG
  cache->cleared = 0;
  cache->inserted = 0;
  cache->hits = 0;
  cache->misses = 0;
#endif
  cache->name = allocMem(cache->name, strlen(name)+1);
  strcpy(cache->name, name);
  cache->data = allocMem(cache, cache->size*cache->entry_size);
  cacheClear(cache);
  return cache;
}

static __inline__ void cacheDone(tCache* cache) {
  freeMem(cache->name);
  freeMem(cache);
}

/* -------------- ApplyCache ----------------- */

extern tCache* m_ApplyCache;

typedef struct {
  tDDD r; /* The result */
  int  o; /* The operator */
  tDDD u; /* The operands */
  tDDD v;
} ApplyCacheNode;

static __inline__ void insertApplyCache(tDDD r, int o, tDDD u, tDDD v) {
  unsigned int hv;
  ApplyCacheNode* e;

  hv = HASH3(o,u,v)%m_ApplyCache->size;
  e = &(((ApplyCacheNode*)m_ApplyCache->data)[hv]);
  e->r = r;
  e->o = o;
  e->u = u;
  e->v = v;
#ifdef DEBUG
  m_ApplyCache->inserted++;
#endif
}
  
static __inline__ bool containsApplyCache(tDDD* r, int o, tDDD u, tDDD v) {
  unsigned int hv;
  ApplyCacheNode* e;

  hv = HASH3(o,u,v)%m_ApplyCache->size;
  e = &(((ApplyCacheNode*)m_ApplyCache->data)[hv]);
  *r = e->r;
#ifdef DEBUG
  if (e->o==o && e->u==u && e->v==v)
    m_ApplyCache->hits++;
  else
    m_ApplyCache->misses++;
#endif
  return e->o==o && e->u==u && e->v==v;
}

/* -------------- PathCountCache ----------------- */

extern tCache* m_PathCountCache;

typedef struct {
  double r;
  tDDD u;
} PathCountCacheNode;

static __inline__ void insertPathCountCache(double r, tDDD u) {
  unsigned int hv;
  PathCountCacheNode* e;

  hv = u%m_PathCountCache->size;
  e = &(((PathCountCacheNode*)m_PathCountCache->data)[hv]);
  e->r = r;
  e->u = u;
#ifdef DEBUG
  m_PathCountCache->inserted++;
#endif
}
  
static __inline__ bool containsPathCountCache(double* r, tDDD u) {
  unsigned int hv;
  PathCountCacheNode* e;

  hv = u%m_PathCountCache->size;
  e = &(((PathCountCacheNode*)m_PathCountCache->data)[hv]);
  *r = e->r;
#ifdef DEBUG
  if (e->u==u)
    m_PathCountCache->hits++;
  else
    m_PathCountCache->misses++;
#endif
  return e->u==u;
}


/* -------------- ReplaceCache ----------------- */

extern tCache* m_ReplaceCache;

typedef struct {
  tDDD r;
  tDDD u;
  tVar x;
  tVar y;
} ReplaceCacheNode;

static __inline__ void insertReplaceCache(tDDD r, tDDD u, tVar x, tVar y) {
  unsigned int hv;
  ReplaceCacheNode* e;

  hv = HASH3(u,x,y)%m_ReplaceCache->size;
  e = &(((ReplaceCacheNode*)m_ReplaceCache->data)[hv]);
  e->r = r;
  e->u = u;
  e->x = x;
  e->y = y;
#ifdef DEBUG
  m_ReplaceCache->inserted++;
#endif
}
  
static __inline__ bool containsReplaceCache(tDDD* r, tDDD u, tVar x, tVar y) {
  unsigned int hv;
  ReplaceCacheNode* e;

  hv = HASH3(u,x,y)%m_ReplaceCache->size;
  e = &(((ReplaceCacheNode*)m_ReplaceCache->data)[hv]);
  *r = e->r;
#ifdef DEBUG
  if (e->u==u && e->x==x && e->y==y)
    m_ReplaceCache->hits++;
  else
    m_ReplaceCache->misses++;
#endif
  return e->u==u && e->x==x && e->y==y;
}


/* -------------- IncrementCache ----------------- */

extern tCache* m_IncrementCache;

typedef struct {
  tDDD r;
  tDDD u;
  tVar x;
  tCstr d;
} IncrementCacheNode;

static __inline__ void insertIncrementCache(tDDD r, tDDD u, tVar x, tCstr d) {
  unsigned int hv;
  IncrementCacheNode* e;

  hv = HASH3(u,x,d)%m_IncrementCache->size;
  e = &(((IncrementCacheNode*)m_IncrementCache->data)[hv]);
  e->r = r;
  e->u = u;
  e->x = x;
  e->d = d;
#ifdef DEBUG
  m_IncrementCache->inserted++;
#endif
}
  
static __inline__ bool containsIncrementCache(tDDD* r, tDDD u, tVar x, tCstr d) {
  unsigned int hv;
  IncrementCacheNode* e;

  hv = HASH3(u,x,d)%m_IncrementCache->size;
  e = &(((IncrementCacheNode*)m_IncrementCache->data)[hv]);
  *r = e->r;
#ifdef DEBUG
  if (e->u==u && e->x==x && e->d==d)
    m_IncrementCache->hits++;
  else
    m_IncrementCache->misses++;
#endif
  return e->u==u && e->x==x && e->d==d;
}


/* -------------- RelaxCache ----------------- */

extern tCache* m_RelaxCache;

typedef struct {
  tDDD r;
  tDDD u;
  tVar z;
  tVars v;
  tCstrOp b;
} RelaxCacheNode;

static __inline__ void insertRelaxCache(tDDD r, tDDD u, tVar z, tVar x, tVar y, tCstr b) {
  unsigned int hv;
  RelaxCacheNode* e;

  unsigned int v = (y<<16) | x;

  hv = HASH4(u,z,v,b)%m_RelaxCache->size;
  e = &(((RelaxCacheNode*)m_RelaxCache->data)[hv]);
  e->r = r;
  e->u = u;
  e->z = z;
  e->v = v;
  e->b = b;
#ifdef DEBUG
  m_RelaxCache->inserted++;
#endif
}
  
static __inline__ bool containsRelaxCache(tDDD* r, tDDD u, tVar z, tVar x, tVar y, tCstr b) {
  unsigned int hv;
  RelaxCacheNode* e;

  unsigned int v = (y<<16) | x;

  hv = HASH4(u,z,v,b)%m_RelaxCache->size;
  e = &(((RelaxCacheNode*)m_RelaxCache->data)[hv]);
  *r = e->r;
#ifdef DEBUG
  if (e->u==u && e->z==z && e->v==v && e->b==b)
    m_RelaxCache->hits++;
  else
    m_RelaxCache->misses++;
#endif
  return e->u==u && e->z==z && e->v==v && e->b==b;
}


/* -------------- ExistsCache ----------------- */

extern tCache* m_ExistsCache;

typedef struct {
  tDDD r;
  tDDD u;
  tVar x;
} ExistsCacheNode;

static __inline__ void insertExistsCache(tDDD r, tDDD u, tVar x) {
  unsigned int hv;
  ExistsCacheNode* e;

  hv = HASH2(u,x)%m_ExistsCache->size;
  e = &(((ExistsCacheNode*)m_ExistsCache->data)[hv]);
  e->r = r;
  e->u = u;
  e->x = x;
#ifdef DEBUG
  m_ExistsCache->inserted++;
#endif
}
  
static __inline__ bool containsExistsCache(tDDD* r, tDDD u, tVar x) {
  unsigned int hv;
  ExistsCacheNode* e;

  hv = HASH2(u,x)%m_ExistsCache->size;
  e = &(((ExistsCacheNode*)m_ExistsCache->data)[hv]);
  *r = e->r;
#ifdef DEBUG
  if (e->u==u && e->x==x)
    m_ExistsCache->hits++;
  else
    m_ExistsCache->misses++;
#endif
  return e->u==u && e->x==x;
}


/* -------------- MergeCache ----------------- */

extern tCache* m_MergeCache;

typedef struct {
  tDDD r;
  tDDD u;
} MergeCacheNode;

static __inline__ void insertMergeCache(tDDD r, tDDD u) {
  unsigned int hv;
  MergeCacheNode* e;

  hv = u%m_MergeCache->size;
  e = &(((MergeCacheNode*)m_MergeCache->data)[hv]);
  e->r = r;
  e->u = u;
#ifdef DEBUG
  m_MergeCache->inserted++;
#endif
}
  
static __inline__ bool containsMergeCache(tDDD* r, tDDD u) {
  unsigned int hv;
  MergeCacheNode* e;

  hv = u%m_MergeCache->size;
  e = &(((MergeCacheNode*)m_MergeCache->data)[hv]);
  *r = e->r;
#ifdef DEBUG
  if (e->u==u)
    m_MergeCache->hits++;
  else
    m_MergeCache->misses++;
#endif
  return e->u==u;
}

/* -------------- PathReduceCache ----------------- */

extern tCache* m_PathReduceCache;

typedef struct {
  tDDD r;
  tDDD u;
} PathReduceCacheNode;

static __inline__ void insertPathReduceCache(tDDD r, tDDD u) {
  unsigned int hv;
  PathReduceCacheNode* e;

  hv = u%m_PathReduceCache->size;
  e = &(((PathReduceCacheNode*)m_PathReduceCache->data)[hv]);
  e->r = r;
  e->u = u;
#ifdef DEBUG
  m_PathReduceCache->inserted++;
#endif
}
  
static __inline__ bool containsPathReduceCache(tDDD* r, tDDD u) {
  unsigned int hv;
  PathReduceCacheNode* e;

  hv = u%m_PathReduceCache->size;
  e = &(((PathReduceCacheNode*)m_PathReduceCache->data)[hv]);
  *r = e->r;
#ifdef DEBUG
  if (e->u==u)
    m_PathReduceCache->hits++;
  else
    m_PathReduceCache->misses++;
#endif
  return e->u==u;
}


/* -------------- Manage all caches ----------------- */

static __inline__ void clearCache(void) {
  cacheClear(m_ApplyCache);
  cacheClear(m_PathCountCache);
  cacheClear(m_ReplaceCache);
  cacheClear(m_IncrementCache);
  cacheClear(m_RelaxCache);
  cacheClear(m_ExistsCache);
  cacheClear(m_MergeCache);
  cacheClear(m_PathReduceCache);
}

static __inline__ void initCache(void) {
  cacheInit(Apply);
  cacheInit(PathCount);
  cacheInit(Replace);
  cacheInit(Increment);
  cacheInit(Relax);
  cacheInit(Exists);
  cacheInit(Merge);
  cacheInit(PathReduce);
}

static __inline__ void doneCache(void) {
  cacheDone(m_ApplyCache);
  cacheDone(m_PathCountCache);
  cacheDone(m_ReplaceCache);
  cacheDone(m_IncrementCache);
  cacheDone(m_RelaxCache);
  cacheDone(m_ExistsCache);
  cacheDone(m_MergeCache);
  cacheDone(m_PathReduceCache);
}

static __inline__ void infoCache(void) {
  cacheInfo(m_ApplyCache);
  cacheInfo(m_PathCountCache);
  cacheInfo(m_ReplaceCache);
  cacheInfo(m_IncrementCache);
  cacheInfo(m_RelaxCache);
  cacheInfo(m_ExistsCache);
  cacheInfo(m_MergeCache);
  cacheInfo(m_PathReduceCache);
}


#endif

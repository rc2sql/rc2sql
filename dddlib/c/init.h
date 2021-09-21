#ifndef INIT_H
#define INIT_H

#include "types.h"
#include "variable.h"

BEGIN_EXPORT
/* Default number of ddd variables, and default sizes of the ddd table
   and the cache. */
#define defaultMaxVar     (1<<10)
#define defaultMbMemUsage 64
#define defaultMemRatio   0.75

/* Initialize the DDD library. */
void init(tVar maxVar, double mb_mem_usage, double mem_ratio);

/* Finalize the DDD library.  All DDD references and variables are
   invalid after the call. */
void done(void);

/* Print various information about the package (some only
   available if the library is compiled with -DDEBUG) */
void info(void);
END_EXPORT

void reinit_vars(tVar maxVar);

#endif

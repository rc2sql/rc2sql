#ifndef MEMORY_H
#define MEMORY_H

/* The memory module implements a simple memory manager.  Blocks of
   memory can be allocated, reallocated, freed, copied and set.  In
   debug mode, a linked list of pointers to all allocated memory
   blocks is maintained, so that operations are only performed on
   valid memory blocks. */

#include <stdlib.h>
#include "types.h"

void initMem(void);
void doneMem(void);
void infoMem(void);

#define allocMem(ptr,size)   allocMemory(size)
#define reallocMem(ptr,size) reallocMemory(ptr, size)

void* allocMemory(size_t size);
void* reallocMemory(void* ptr, size_t size);

/* Free the memory at ptr */
void freeMem(void* ptr);

/* Copy memory areas.  copyAllMem copies between blocks of equal size
   (which must be the full size of the block). */
void* copyMem(void* dest, const void* src, size_t size);
void* copyAllMem(void* dest, const void* src, size_t size);

/* Fill s bytes of memory at p with a constant *byte* v  */
void* setMem(void* p, int v, size_t s);

#endif

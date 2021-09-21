#include <string.h>
#include "types.h"
#include "memory.h"
#include "io.h"

#ifdef DEBUG

struct tMemBlock {
  /* Pointer to the allocated memory */
  const void* p;
  /* The size of the allocated memory */
  size_t size;
  /* Next memory block in list */
  struct tMemBlock* next;
  /* Previous memory block in list */
  struct tMemBlock* prev;
};

/* Memory blocks are maintained in a doubly linked list */
static struct tMemBlock* m_head;

/* Insert a new memory block (p,size) in front of the list */
static void insertMemBlock(const void* p, size_t size) {
  struct tMemBlock* block;
  block = (struct tMemBlock*) malloc(sizeof(struct tMemBlock));

  if (block==NULL)
    m_errorHandler(MSG_ALLOC_MEMORY,
		   sizeof(struct tMemBlock), "insertMemBlock");

  block->next = m_head;
  block->prev = NULL;

  /* If block is not the only element in the list */
  if (block->next!=NULL)
    m_head->prev = block;
  m_head = block;

  /* Insert the data*/
  block->p = p;
  block->size = size;
}

/* Find the memory block with pointer p */
static struct tMemBlock* findMemBlock(const void* p) {
  struct tMemBlock *block = m_head;
  while (block!=NULL) {
    if (block->p == p)
      return block;
    block = block->next;
  }
  return NULL;
}

/* Delete the memory block with pointer p */
static void deleteMemBlock(const void* p) {
  struct tMemBlock *block;
  if ((block = findMemBlock(p)) != NULL) {
    if (block->prev == NULL) {
      /* the first element */
      m_head = block->next;
    }
    else {
      block->prev->next = block->next;
    }

    if (block->next != NULL) {
      /* not the last element */
      block->next->prev = block->prev;
    }
    free(block);
  }
  else
    m_errorHandler(MSG_MEM_NOT_FOUND,p);
}

static size_t getSizeOf(const void* p) {
  struct tMemBlock* block;
  if ((block = findMemBlock(p)) == NULL)
    m_errorHandler(MSG_MEM_NOT_FOUND,p);
  return block->size;
}

void* allocMemory(size_t size) {
  void* ptr = malloc(size);
  if (ptr==NULL)
    m_errorHandler(MSG_ALLOC_MEMORY, size);
  insertMemBlock(ptr, size);
  return ptr;
}

void* reallocMemory(void* ptr, size_t size) {
  void* newptr;
  /* Allow realloc'ing a NULL pointer */
  if (ptr!=NULL)
    deleteMemBlock(ptr);
  newptr = realloc(ptr, size);
  if (newptr==NULL)
    m_errorHandler(MSG_ALLOC_MEMORY, size);
  insertMemBlock(newptr, size);
  return newptr;
}

#else

void* allocMemory(size_t size) {
  void* ptr = malloc(size);
  if (ptr==NULL)
    m_errorHandler(MSG_ALLOC_MEMORY, size);
  return ptr;
}

void* reallocMemory(void* ptr, size_t size) {
  void* newptr;
  newptr = realloc(ptr, size);
  if (newptr==NULL)
    m_errorHandler(MSG_ALLOC_MEMORY, size);
  return newptr;
}

#endif


void initMem(void) {
#ifdef DEBUG
  m_head = NULL;
#endif
}

#ifdef DEBUG
static void dumpMem(void) {
  struct tMemBlock* tmp;
  tmp = m_head;
  printMsg("\n");
  while (tmp!=NULL) {
    printf("%10d bytes at %10p.\n",
	   tmp->size, tmp->p);
    tmp = tmp->next;
  }
  printMsg("\n");
}
#endif

void doneMem(void) {
#ifdef DEBUG
  if (m_head!=NULL) {
    printMsg("Memory leaks:\n");
    dumpMem();
  }
#endif
}

void infoMem(void) {
#ifdef DEBUG
  dumpMem();
#endif
}

/* Free the memory at p */
void freeMem(void* p) {
#ifdef DEBUG
  deleteMemBlock(p);
#endif
  free(p);
  p = NULL;
}

void* copyMem(void* dest, const void* src, size_t size) {
  return memcpy(dest,src,size);
}

void* copyAllMem(void* dest, const void* src, size_t size) {
#ifdef DEBUG
  size_t dest_size, src_size;
  dest_size = getSizeOf(dest);
  src_size = getSizeOf(src);
  if (!(dest_size == src_size && src_size==size)) {
    m_errorHandler(MSG_SIZE_MISMATCH,size);
  }
#endif
  return memcpy(dest,src,size);
}

/* Set memory */
void* setMem(void* dest, int value, size_t size) {
  return memset(dest,value,size);
}




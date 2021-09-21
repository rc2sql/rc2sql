#include <stdarg.h>
#include "types.h"

bool m_isInitialized = false;
unsigned int m_tableSize;

unsigned int m_cache_size;
unsigned int m_mem_size;

/* Structure of a message */
typedef struct {
  tMessageId  m_id;    /* Id */
  const char* m_str;   /* Message string */
} tMessage;


static const tMessage messageTable[] = {
  { MSG_UNKNOWN,
    "Internal error:  An unknown error occurred." },
  { MSG_COMPARE_ERROR,
    "Internal error:  Error in compareVarsCstrOp." },
  { MSG_MEM_NOT_FOUND,
    "Internal error: "
    "The memory at '%p' has not been allocated with allocMem." },
  { MSG_SIZE_MISMATCH,
    "Internal error: Size mismatch in memory operation (%p)." },
  { MSG_NOT_IMPLEMENTED,
    "Internal error: Function not implemented in core library: '%s'." },
  { MSG_MEMORY_INFO,
    "%10d bytes at %10p.\n" },
  { MSG_NO_DDD_ENTRIES,
    "Memory exhausted.\n"
    "Call init() with a tableSize larger than %d." },
  { MSG_OUT_OF_VARS,
    "Cannot create variable %d.\n"
    "Call init() with a maxVar larger than %d."},
  { MSG_ALLOC_MEMORY,
    "Out of memory (allocation of %d bytes failed)." },
  { MSG_OPEN_FILE,
    "Error opening file '%s'." },
  { MSG_EXECUTE_PRG,
    "Error executing '%s'.\n"
    "This might be because 'sh' is not a link to 'bash' or 'tcsh'." }
};

static const unsigned int messageTableSize = sizeof(messageTable)/sizeof(tMessage);


/* Return the index of the entry in table that has id, or 0 if the
   table has no such entry (using binary search) */
tMessage findMessage(tMessageId id) {
  unsigned int left, right, middle;
  tMessageId currentId;

  left = 0;
  right = messageTableSize-1;
  while (left <= right) {
    middle = (left + right) / 2;
    currentId = messageTable[middle].m_id;
    if (currentId < id)
      left = middle + 1;
    else if (currentId > id)
      right = middle - 1;
    else
      return messageTable[middle];
  }
  return messageTable[MSG_UNKNOWN];
}

const char* messageStr(tMessageId id) {
  return findMessage(id).m_str;
}

void defaultErrorHandler(tMessageId id, ...) {
  va_list arglist;
  va_start(arglist, id);
  
  printf("Error (%u): ", id);
  vprintf(messageStr(id), arglist);
  printf("\n");
  va_end(arglist);

  exit(1);
}

tErrorHandler m_errorHandler = defaultErrorHandler;

void set_error_handler(tErrorHandler f) {
  m_errorHandler = f;
}

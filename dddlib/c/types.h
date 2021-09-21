#ifndef TYPES_H
#define TYPES_H

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <limits.h>


/* BEGIN_EXPORT and END_EXPORT expand to nothing, but the makeheader program
   uses them to generate a ddd.h header file for the ddd library.  */
#define BEGIN_EXPORT
#define END_EXPORT

BEGIN_EXPORT
#ifndef __cplusplus
   typedef int bool;
#  define false 0
#  define true (!false)
#endif

/* From malloc.h */
#ifndef NULL
#ifdef __cplusplus
#define NULL    0
#else
#define NULL    ((__malloc_ptr_t) 0)
#endif
#endif

/* A DDD is represented as an integer.  */
typedef unsigned int tDDD;
END_EXPORT

#define DDDIDSTRING \
  "The DDD-library\nVersion 1.0\n" \
  "Copyright (C) 1998-2002 Jesper Moller <jm@configit-software.com> and Jakob Lichtenberg <jl@configit-software.com>\n" \
  "ConfigIt Software A/S"

/* Define DEBUG if not in NDEBUG mode */
#ifndef NDEBUG
#  define DEBUG
#endif

/* Test whether >> shifts in sign bits */
#if( ( ((-1) >> 1) == -1 ) )
#  define SHR_IS_ARITHMETIC
#else
#  undef  SHR_IS_ARITHMETIC
#endif

#ifdef __GNUC__

/* Tell GNU C not to warn about unused functions and hint about
   functions that are 'pure' in the sense that they only depend on
   their arguments, and have no side-effects (except for the return
   value). */
#  define ATTR_UNUSED        __attribute__ ((unused))
#  define ATTR_CONST         __attribute__ ((const))
#else
#  define ATTR_UNUSED
#  define ATTR_CONST
#endif

#define PRECONDITION(p) assert(p)

/* True if the ddd library has been initialized */
extern bool m_isInitialized;

/* The number of entries in the ddd table */
extern unsigned int m_tableSize;

/* The total number of bytes for cache and ddd entries */
extern unsigned int m_cache_size;
extern unsigned int m_mem_size;

BEGIN_EXPORT
/* Message identifiers */
typedef enum {                                   
  MSG_UNKNOWN           =   0,
  MSG_COMPARE_ERROR     =  10,
  MSG_MEM_NOT_FOUND     =  11,
  MSG_SIZE_MISMATCH     =  12,
  MSG_NOT_IMPLEMENTED   =  13,
  MSG_MEMORY_INFO       =  14,
  MSG_NO_DDD_ENTRIES    =  20,
  MSG_OUT_OF_VARS       =  21,
  MSG_ALLOC_MEMORY      =  30,
  MSG_OPEN_FILE         =  31,
  MSG_EXECUTE_PRG       =  32
} tMessageId;

/* The message string of a message */
const char* messageStr(tMessageId id);

/* Type of error handler functions */
typedef void (*tErrorHandler)(tMessageId, ...);

/* Set error handling function */
void set_error_handler(tErrorHandler f);

END_EXPORT

/* The error handling function */
extern tErrorHandler m_errorHandler;

static __inline__ unsigned int HASH2(unsigned int a,
				     unsigned int b) ATTR_UNUSED ATTR_CONST;

static __inline__ unsigned int HASH3(unsigned int a,
				     unsigned int b,
				     unsigned int c) ATTR_UNUSED ATTR_CONST;

static __inline__ unsigned int HASH4(unsigned int a,
				     unsigned int b,
				     unsigned int c,
				     unsigned int d) ATTR_UNUSED ATTR_CONST;


/* ----- Inline definition ----- */

static __inline__ unsigned int HASH2(unsigned int a,
				     unsigned int b) {
  return (a+b)*(a+b+1)/2+a;
}

static __inline__ unsigned int HASH3(unsigned int a,
				     unsigned int b,
				     unsigned int c) {
  return HASH2(HASH2(a,b),c);
}

static __inline__ unsigned int HASH4(unsigned int a,
				     unsigned int b,
				     unsigned int c,
				     unsigned int d) {
  return HASH2(HASH2(a,c),HASH2(b,d));
}

#endif

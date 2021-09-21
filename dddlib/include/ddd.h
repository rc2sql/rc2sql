#ifndef DDD_H
#define DDD_H


/* ------------ types.h ------------ */

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



/* ------------ variable.h ------------ */

/* A variable is represented by an integer. */
typedef short unsigned int tVar; 

/* Create an integer- or real-valued variable with the given name.
   Variable names are used in pretty printing DDDs, so names should
   differ.  The variable ordering is the order of creation. */
tVar mkVar(const char* name);

/* Create a Boolean variable with the given name (like mkVar). */
tVar mkBool(const char* name);

/* The name of a variable */
const char* nameOfVar(tVar v);

/* Constants in upper bounds are signed integers.  Because of the
   encoding of upper bouds, an constant should only contain 31 bits
   (1 sign bit and 30 data bits). */
typedef int tCstr;

#define CSTR_MIN (-((1<<30)-1))
#define CSTR_MAX ((1<<30)-1)

/* Relational operators used in difference constraint. */
typedef enum {
  EQ  = 0,     /* == */
  NEQ = 1,     /* != */
  LEQ = 2,     /* <= */
  GEQ = 3,     /* >= */
  LE  = 4,     /* <  */
  GR  = 5      /* >  */
} tComp;

/* Set (x,y) as the last variable pair in the ordering */
void appendRealToOrder(tVar x, tVar y);

/* Set x as the last variable pair in the ordering */
void appendBoolToOrder(tVar x);

/* Set (x,y) as the first variable pair in the ordering */
void prependRealToOrder(tVar x, tVar y);

/* Set x as the first variable pair in the ordering */
void prependBoolToOrder(tVar x);

/* Order the (remainig) variable pairs */
void appendIncrLexToOrder(void);
void prependIncrLexToOrder(void);
void appendDecrLexToOrder(void);
void prependDecrLexToOrder(void);

/* Clear the order of all variables */
void clearOrder(void);
void printOrder(void);



/* ------------ functions.h ------------ */

/* The DDDs representing false and true, respectively */
extern const tDDD ff;
extern const tDDD tt;

/* Construct the DDD for each of the six types of difference
   constraints 'p-n o c', where p and n are variables created with
   mkVar, o is a relational comparator, and c is a constant.  The
   variables must be different. */
tDDD mkDiffCstr(tVar p, tVar n, tComp o, tCstr c);

/* Construct the DDD for the Boolean variable b.  */
tDDD mkBoolVar(tVar b);

/* Combine DDDs with Boolean connectives */
tDDD nor(tDDD u, tDDD v);
tDDD nimp(tDDD u, tDDD v);
tDDD xor(tDDD u, tDDD v);
tDDD nand(tDDD u, tDDD v);
tDDD and(tDDD u, tDDD v);
tDDD biimp(tDDD u, tDDD v);
tDDD imp(tDDD u, tDDD v);
tDDD or(tDDD u, tDDD v);
tDDD not(tDDD);

/* Existential quantification */
tDDD exists(tVar x, tDDD u);

/* Universal quantification */
tDDD forall(tVar x, tDDD u);

/* Assign x to y+c in u, where x and y are non-Boolean variables
   (i.e., u[x <- y+c]). */
tDDD assign(tDDD u, tVar x, tVar y, tCstr c);

/* Assign x to b (true or false) in u, where x is a Boolean
   variable (i.e., u[x <- b]). */
tDDD assignBool(tDDD u, tVar x, unsigned short int b);

/* Replace all occurrences of y in u by x (i.e., u[x/y]). */
tDDD replace(tDDD u, tVar x, tVar y);

/* True iff u is true in all assignments */
bool tautology(tDDD u);

/* True iff u is true in some assignment */
bool satisfiable(tDDD u);

/* True iff u is false in all assignments */
bool unsatisfiable(tDDD u);

/* True iff u is false in some assignment */
bool falsifiable(tDDD u);

/* True iff u and v are semantically equivalent */
bool equivalent(tDDD u, tDDD v);

/* True iff v is a logical consequence of u */
bool consequence(tDDD u, tDDD v);

/* Remove all infeasible paths in u */
tDDD pathReduce(tDDD u);

/* Remove all mergeable vertices in u */
tDDD merge(tDDD u);

/* Saturate u */
tDDD saturate(tDDD u);

/* Return the convex hull of u (i.e., the minimum and maximum
   differences between all variables in u). */
tDDD hull(tDDD u);

/* Print a satisfying variable assignment of u (if one exists). */
void anysat(tDDD u);

/* Count the number of vertices in u */
unsigned int nodeCount(tDDD u);

/* Count the number of 1-paths in u */
double pathCount(tDDD u);

/* Count the number of different variables in u. */
unsigned int varCount(tDDD u);

/* Returns true if the variable x is free in u */
bool isFreeVar(tVar x, tDDD u);


/* ------------ table.h ------------ */

/* Increment u's reference count (protecting it from garbage
   collection). */
void incRef(tDDD u);

/* Decrement u's reference count (making it recyclable if not
   referenced anymore). */
void decRef(tDDD u);

/* Type of user-defined garbage collection functions. */
typedef void (*tPreGC)(void);
void set_pre_gc(tPreGC f);


/* ------------ io.h ------------ */

/* Print u as disjunctive normal form interpreting z as zero. */
void printDNF(tDDD u, tVar z);

/* Print u in ITE normal form. */
void printINF(tDDD u);

/* Print u in ITE normal form (except for some primitive level of
   pretty printing). */
void printINFpretty(tDDD u);

/* Save u to a file. */
void save_to_file(const char* name, tDDD u);

/* Restore u to a file. */
tDDD restore_from_file(const char* name);

/* Save the graph of u (dot format) */ 
void saveGraph(const char* dotname, tDDD u);

/* View the graph of u using dot and gv */ 
void viewGraph(tDDD u);

/* Save an (x1,x2)-plot of u for x0=0 (PSTricks format) */ 
void savePlot(const char* texname, tVar x0, tVar x1, tVar x2, tDDD u);

/* View an (x1,x2)-plot of u for x0=0 latex, dvips and gv */
void viewPlot(tVar x0, tVar x1, tVar x2, tDDD u);

/* If true, each node in the graph is numbered */
void setGraphOptions(bool saveAsDot, bool graphNumbering);
void setGraphName(const char* name);

/* Default graph parameters */
#define defaultSaveAsDot true
#define defaultGraphNumbering false

/* The different colours that can be used in plotting */
typedef enum {
  BLACK     =  0,
  DARKGRAY  =  1,
  GRAY      =  2,
  LIGHTGRAY =  3,
  WHITE     =  4,
  RED       =  5,
  GREEN     =  6,
  BLUE      =  7,
  CYAN      =  8,
  MAGENTA   =  9,
  YELLOW    = 10
} tColour;

/* Set plotting parameters (only if you don't want to use the default
   parameters shown below). */

/* Set the minimum and maximum values of the two axis */
void setPlotWindow(int xMin, int yMin, int xMax, int yMax);

/* Set the size (in millimeters) of the plot */
void setPlotSize(int width, int height);

/* Set axis options */
void setPlotAxis(bool showAxis, bool showNumbers,
		 bool showTicks, bool showAxisInFront);

/* Plot edged around each convex region */
void setPlotEdges(bool showEdges);

/* Set the colors to be used */
void setPlotColours(tColour fillColour, tColour edgeColour);

/* If saveAsArticle is used, the ps-tricks code is wrapped in a LaTeX
   article. */
void setPlotSaveOptions(bool saveAsArticle);

/* Pass ps-tricks options (these are put right before the
   \endpsfigure) */
void setPlotPsCode(const char* psCode);

/* Default plotting parameters */
#define defaultxMin            -5 
#define defaultxMax             5
#define defaultyMin            -5 
#define defaultyMax             5

#define defaultWidth           100
#define defaultHeight          100

#define defaultShowEdges       true

#define defaultShowAxis        true
#define defaultShowNumbers     true
#define defaultShowTicks       true
#define defaultShowAxisInFront true

/* The default colours are the most brightening we could think of :-) */
#define defaultFillColour      LIGHTGRAY
#define defaultEdgeColour      BLACK

#define defaultSaveAsArticle   false


/* ------------ init.h ------------ */

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
#endif /* DDD_H */

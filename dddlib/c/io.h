#ifndef IO_H
#define IO_H

#include "types.h"
#include "variable.h"

BEGIN_EXPORT
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
END_EXPORT

/* Printing (like printf) */
void printMsg(const char*, ...);

/* Print the variable v (or variable pair) */
void printVar(tVar v);
void printVars(tVars v);

/* Print the constraint c */
void printCstr(tCstr c);

/* Print the operator o */
void printOp(tOp c);

/* Print the constraint and operator c */
void printCstrOp(tCstrOp c);

void initIo(void);
void doneIo(void);

#endif

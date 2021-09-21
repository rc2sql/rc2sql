#include <string.h>
#include <stdarg.h>
#include "io.h"
#include "table.h"
#include "variable.h"
#include "functions.h"
#include "memory.h"

/* Graph parameters */
static const char graphHeader[] =
"digraph \"%s\" {\n"
"  fontsize = \"8\"\n"
"  size = \"6.5,9\"\n"
"  ratio = compress;\n";

static const char graphFooter[] = "}\n";

static bool m_saveAsDot;
static bool m_graphNumbering;
static bool m_graphBoolSameRank;
static char* m_graphName;

/* Plot parameters */
static const char m_articleHeader[] =
"\\documentclass{article}\n"
"\\pagestyle{empty}\n"
"\\usepackage{pst-plot}\n"
"\\begin{document}\n";

static const char m_articleFooter[] =
"\\end{document}\n";

static const char m_figureHeader[] = "";
static const char m_figureFooter[] = "";

static const char *m_colours[] = 
{ "black", "darkgray", "gray", "lightgray", "white",
  "red", "green", "blue", "cyan", "magenta", "yellow"};

static const char m_edgeWidth[] = "2pt";

/* choose "" or "{->}" or "{<->}" */
static const char m_arrows[] = "{->}";

static int m_xMin;
static int m_yMin;
static int m_xMax;
static int m_yMax;

static int m_height;
static int m_width;

static tColour m_fillColour;
static tColour m_edgeColour;

static bool m_showEdges;

static bool m_showAxis;
static bool m_showNumbers;
static bool m_showTicks;
static bool m_showAxisInFront;

static bool m_savePlotAsArticle;
static char* m_psCode;

/* Initialization */
void initIo(void) {
  setGraphOptions(defaultSaveAsDot,defaultGraphNumbering);
  m_graphBoolSameRank = true;
  setPlotWindow(defaultxMin, defaultyMin,
		defaultxMax, defaultyMax);
  setPlotSize(defaultWidth, defaultHeight);
  setPlotAxis(defaultShowAxis, defaultShowNumbers,
	      defaultShowTicks, defaultShowAxisInFront);
  setPlotEdges(defaultShowEdges);
  setPlotColours(defaultFillColour, defaultEdgeColour);
  setPlotSaveOptions(defaultSaveAsArticle);

  m_psCode = allocMem(m_psCode,1);
  strcpy(m_psCode,"");

  m_graphName = allocMem(m_graphName, 2);
  strcpy(m_graphName, "G");
}

void doneIo(void) {
  freeMem(m_psCode);
}

/* Some auxiliary functions */
static void executeCmd(const char* cmd, ...) {
  char buf[1024];
  va_list arglist;
  va_start(arglist, cmd);
  vsprintf(buf, cmd, arglist);
  if (system(buf)!=0)
    m_errorHandler(MSG_EXECUTE_PRG, buf);
  va_end(arglist);
}

/* General printing to streams */
static void fprintVar(FILE* s, tVar v) {
  fprintf(s, "%s", nameOfVar(v));
}

static void fprintVars(FILE* s, tVars v) {
  fprintf(s, "%s - %s",
	  nameOfVar(posOfVars(v)), nameOfVar(negOfVars(v)));
}

static void fprintOp(FILE* s, tOp o) {
  static const char* comp[] = { "<", "<=" }; 
  PRECONDITION(isValidOp(o));
  fprintf(s, "%s", comp[o]);
}

static void fprintCstr(FILE* s, tCstr v) {
  PRECONDITION(isValidCstr(v));

  if (v==CSTR_MAX)
    fprintf(s, "oo");
  else
    fprintf(s, "%d", v);
}

static void fprintCstrOp(FILE* s, tCstrOp c) {
  static const char* comp[] = { "<", "<=" }; 

  PRECONDITION(isValidCstrOp(c));

  if (c==oo)
    fprintf(s, "<= oo");
  else
    fprintf(s, "%s %d", comp[opOfCstrOp(c)], valueOfCstrOp(c));
}

/* General printing to stdout */
void printVar(tVar v) {
  fprintVar(stdout, v);
}

void printVars(tVars v) {
  fprintVars(stdout, v);
}

void printOp(tOp o) {
  fprintOp(stdout, o);
}

void printCstr(tCstr v) {
  fprintCstr(stdout, v);
}

void printCstrOp(tCstrOp c) {
  fprintCstrOp(stdout, c);
}

void printMsg(const char* s, ...) {
  va_list arglist;
  va_start(arglist, s);
  vprintf(s, arglist);
  va_end(arglist);
}


/* Constructing Graphs */

void setGraphOptions(bool saveAsDot, bool graphNumbering) {
  m_saveAsDot = saveAsDot;
  m_graphNumbering = graphNumbering;
}

void setGraphName(const char* name) {
  PRECONDITION(m_graphName!=NULL);

  m_graphName = reallocMem(m_graphName, strlen(name)+1);
  strcpy(m_graphName, name);
}



static void mkBoolSameRankAux(FILE* s, tDDD u, tVar v) {
  if (u<=1 || mark(u)==1)
    return;
  setMark(u);
  if (pos(u)==v) {
    fprintf(s, "  \"%d\";\n", u);
  }
  else {
    mkBoolSameRankAux(s, low(u),v);
    mkBoolSameRankAux(s, high(u),v);
  }
}

static void mkBoolSameRank(FILE* s, tDDD u) {
  tVar v;
  for (v=0; v<m_curVar; v++) {
    if (typeOfVar(v)==BOOLEAN) {
      fprintf(s, "{ rank=same;\n");
      mkBoolSameRankAux(s,u,v);
      fprintf(s, "};\n");
      unmarkNode(u);
    }
  }
}

static void mkGraphAux(FILE* s, tDDD u) {
  static const char* compTex[] = { "<", "\\leq" }; 

  PRECONDITION(isValidDDD(u));
  
  if (u<=1 || mark(u)==1)
    return;
  
  setMark(u);

  fprintf(s, "%d [label = \"", u); /* Add fx: height = 3, width = 3, */
  
  if (m_graphNumbering)
    fprintf(s, "%d : ", u);


  if (typeOfVar(pos(u))==BOOLEAN) {
    PRECONDITION(typeOfVar(neg(u))==BOOLEAN);
    fprintVar(s, pos(u));
  }
  else {
    fprintVars(s, vars(u));
    fprintf(s, " ");
    if(m_saveAsDot)
      fprintCstrOp(s, cstrOp(u));
    else
      fprintf(s, "%s %d", compTex[opOfCstrOp(cstrOp(u))], 
	      valueOfCstrOp(cstrOp(u)));
  }
  fprintf(s, "\"]\n");

  if (low(u)!=0)
    fprintf(s, "%d -> %d [style=dashed,color=red];\n", u, low(u));
  if (high(u)!=0)
    fprintf(s, "%d -> %d [style=solid,color=blue];\n", u, high(u));

  mkGraphAux(s, low(u));
  mkGraphAux(s, high(u));
}

static void mkGraph(FILE* s, tDDD u) {
  static char* term[] =
  { "  0 [shape=box];\n",
    "  1 [shape=box];\n" };

  PRECONDITION(isValidRefDDD(u));

  fprintf(s, graphHeader, m_graphName);
  //  fprintf(s, "  %s [shape=box];\n", m_graphName);

  if (u<=1) {
    fprintf(s, term[u]);
  }
  else {
    fprintf(s, term[1]);
    if (m_graphBoolSameRank)
      mkBoolSameRank(s,u);
    mkGraphAux(s,u);
    unmarkNode(u);
  }
  fprintf(s, graphFooter);
}

void saveGraphDot(const char* dotname, tDDD u) {
  FILE* dothandle;

  if ((dothandle = fopen(dotname, "w"))==NULL)
    m_errorHandler(MSG_OPEN_FILE, dotname);
  mkGraph(dothandle,u);
  fclose(dothandle);
}

void saveGraph(const char* name, tDDD u) {
  char dotname[1024];
  char *prefixname;
  if (m_saveAsDot)
    saveGraphDot(name,u);
  else {
    prefixname = tmpnam(NULL);
    sprintf(dotname,"%s.dot",prefixname);
    saveGraphDot(dotname,u);
    executeCmd("dotex %s %s", dotname, name);
    executeCmd("rm -f %s %s", dotname);
  }
}

/* Viewing graphs using dot directly */
void viewGraph(tDDD u) {
  char dotname[1024];
  char psName[1024];
  char *prefixname;

  prefixname = tmpnam(NULL);
  sprintf(dotname,"%s.dot",prefixname);
  sprintf(psName,"%s.ps",prefixname);

  saveGraph(dotname, u);
  executeCmd("dot -Tps -o %s %s", psName, dotname);
  remove(dotname);
  
  /* Works with bash and tcsh (that is: sh must be a link to bash or tcsh */
  executeCmd("(ghostview %s ; rm -f %s) &", psName,psName); 
}

/* Viewing graphs using dot and dotex */
void viewGraphDotex(tDDD u) {
  static char dotName[] = "tmp.dot";
  static char plainName[] = "tmp.plain";
  static char texName[] = "tmp.tex";
  static char dviName[] = "tmp.dvi";
  static char psName[] = "tmp.ps";
  char *dirName;
  char dotfullname[1024];

  dirName = tmpnam(NULL);
  sprintf(dotfullname, "%s/%s",dirName,dotName);

  executeCmd("mkdir %s", dirName);
  saveGraph(dotfullname, u);
  executeCmd("cd %s; dot -Tplain -o %s %s", dirName, plainName, dotName);
  executeCmd("cd %s; dotex %s %s", dirName, plainName, texName);
  executeCmd("cd %s; latex %s", dirName, texName);
  executeCmd("cd %s; dvips -o %s %s", dirName, psName, dviName);

  /* Works with bash and tcsh (that is: sh must be a link to bash or tcsh */
  executeCmd("((cd %s; ghostview %s) ; rm -rf %s) &", dirName,psName, dirName);
}

/* Constructing Plots */
void setPlotWindow(int xMin, int yMin, int xMax, int yMax) {
  m_xMin = xMin;
  m_xMax = xMax>=xMin ? xMax : xMin;
  m_yMin = yMin;
  m_yMax = yMax>=yMin ? yMax : yMin;
}

void setPlotSize(int width, int height) {
  m_width = width>=0 ? width : 0;
  m_height = height>=0 ? height : 0;
}

void setPlotAxis(bool showAxis, bool showNumbers,
		 bool showTicks, bool showAxisInFront) {
  m_showAxis = showAxis;
  m_showNumbers = showNumbers;
  m_showTicks = showTicks;
  m_showAxisInFront = showAxisInFront;
}

void setPlotEdges(bool showEdges) {
  m_showEdges = showEdges;
}

void setPlotColours(tColour fillColour, tColour edgeColour) {
  assert(BLACK<=fillColour && fillColour<=YELLOW);
  assert(BLACK<=edgeColour && edgeColour<=YELLOW);

  m_fillColour = fillColour;
  m_edgeColour = edgeColour;
}

void setPlotSaveOptions(bool saveAsLatexArticle) {
  m_savePlotAsArticle = saveAsLatexArticle;
}

void setPlotPsCode(const char* psCode) {
  PRECONDITION(m_psCode!=NULL);

  m_psCode = reallocMem(m_psCode, strlen(psCode)+1);
  strcpy(m_psCode, psCode);
}


static void mkCstr(FILE* s, int r, tCstrOp c, int p1, int p2,
		   int p3, int p4, int p5, int p6, int p7, int p8) {

  static const char noneLine[] = 
    "  \\psline[linestyle=none](%d,%d)(%d,%d)(%d,%d)(%d,%d)\n";

  static const char customLine[] = 
    "\\psline[linestyle=%s,linewidth=%s,linecolor=%s](%d,%d)(%d,%d)\n";

  PRECONDITION(r==1 || r==2); /* Round can be 1 or 2 */
  PRECONDITION(opOfCstrOp(c)==COMP_LESSEQ || opOfCstrOp(c)==COMP_LESS);

  if (c!=oo) {
    if (r==1)
      fprintf(s,noneLine,p1,p2,p3,p4,p5,p6,p7,p8);
    else {
      if (m_showEdges)
	fprintf(s,customLine, opOfCstrOp(c)==COMP_LESSEQ ? "solid" : "dashed",
		m_edgeWidth, m_colours[m_edgeColour], p1,p2,p3,p4);
    }
  }
}

static void mkConvex(FILE* s, int r, tCstrOp x1x0, tCstrOp x0x1,
		   tCstrOp x2x0, tCstrOp x0x2, tCstrOp x2x1, tCstrOp x1x2) {
  tCstr v;
  tCstrOp c;

  c = x1x0; 
  v = valueOfCstrOp(c);
  mkCstr(s, r, c, v, m_yMin, v, m_yMax,
	 m_xMin, m_yMax, m_xMin, m_yMin); 

  c = x0x1;
  v = valueOfCstrOp(c);
  mkCstr(s, r, c, -v, m_yMin, -v, m_yMax, 
	 m_xMax, m_yMax, m_xMax, m_yMin); 

  c = x2x0; 
  v = valueOfCstrOp(c);
  mkCstr(s, r, c, m_xMin, v, m_xMax, v,
	 m_xMax, m_yMin, m_xMin, m_yMin); 

  c = x0x2;
  v = valueOfCstrOp(c);
  mkCstr(s, r, c, m_xMin, -v, m_xMax, -v,
	 m_xMax, m_yMax, m_xMin, m_yMax); 

  c = x2x1;
  v = valueOfCstrOp(c);
  mkCstr(s, r, c, m_xMin, m_xMin+v, m_xMax, m_xMax+v,
	 m_xMax, m_yMin, m_xMin, m_yMin);

  c = x1x2;
  v = valueOfCstrOp(c);
  mkCstr(s, r, c, m_xMin, m_xMin-v, m_xMax, m_xMax-v,
	 m_xMax, m_yMax, m_xMin, m_yMax);
}
    
static void mkPlotAux(FILE* s, tVar x0, tVar x1, tVar x2, tDDD top, tDDD v) {
  PRECONDITION(isValidDDD(v));

  switch(v) {
  case 0:
    break;
  case 1: {
    tDDD w = top;
    tVar m,n;
    tCstrOp d;

    tCstrOp x1x0 = oo; /* x1 -- x0 <= x1x0 */
    tCstrOp x0x1 = oo; /* x0 -- x1 <= x0x1 */
    tCstrOp x2x0 = oo; /* x2 -- x0 <= x2x0 */
    tCstrOp x0x2 = oo; /* x0 -- x2 <= x0x2 */
    tCstrOp x2x1 = oo; /* x2 -- x1 <= x2x1 */
    tCstrOp x1x2 = oo; /* x1 -- x2 <= x1x2 */
  
    w = top;
      
    /* Run through the onepath using the Path Finder Protocol */
    while (w!=1) {
      assert(w!=0);
      
      if (path(w)==0) { /* A negated constraint */
	m = neg(w);
	n = pos(w);
	d = negCstrOp(cstrOp(w));
	w = low(w);     /* Next iteration, Follow the low son */
      }
      else {            /* A positive constraint */
	m = pos(w);
	n = neg(w);
	d = cstrOp(w);
	w = high(w);    /* Next iteration, Follow the high son */
      }

      if      (m==x1 && n==x0) x1x0 = d; 
      else if (m==x0 && n==x1) x0x1 = d;
      else if (m==x2 && n==x0) x2x0 = d;
      else if (m==x0 && n==x2) x0x2 = d;
      else if (m==x2 && n==x1) x2x1 = d;
      else if (m==x1 && n==x2) x1x2 = d;
      else assert(0);
      
    } /* end while */

    /* Now, the one-path consists of UPTO six constraints. */
      
    /* start the clipper */
    fprintf(s,"\\psclip{\n");

    /* Plot the constraints */
    mkConvex(s,1,x1x0,x0x1,x2x0,x0x2,x2x1,x1x2);

    /* Conjunct them together */
    fprintf(s,"  \\psframe*[linecolor=%s](%d,%d)(%d,%d)\n",
	    m_colours[m_fillColour], m_xMin, m_yMin, m_xMax, m_yMax);
    
    /* Close the clip */
    fprintf(s,"  }\n");
  
    /* Plot the solid or dotted lines in the clip */
    mkConvex(s,2,x1x0,x0x1,x2x0,x0x2,x2x1,x1x2);

    /* Stop the clipper */
    fprintf(s,"\\endpsclip\n");
  
    break;
  }

  default:
    /* Use the Path Finder Protocol when visiting low(v) */
    assert(path(v)==0);
    mkPlotAux(s, x0, x1, x2, top, low(v));
    
    /* Use the Path Finder Protocol when visiting high(v) */
    setPath(v);
    mkPlotAux(s, x0, x1, x2, top, high(v));
    resetPath(v);
    break;
  }
}

static void mkPlot(FILE* s, tVar x0, tVar x1, tVar x2, tDDD u) {
  tVar i;
  tDDD t1, t2;

  PRECONDITION(isValidVar(x0));
  PRECONDITION(isValidVar(x1));
  PRECONDITION(isValidVar(x2));
  PRECONDITION(isValidRefDDD(u));

  if (x0==x1 || x1==x2 || x2==x0) {
    printf("Variables must differ.\n");
    exit(1);
  }

  t1 = u;
  inlineIncRef(t1);

  /* Free all variables but x0, x1 and x2 */
  for (i=0; i<m_curVar; i++) {
    if (i!=x0 && i!=x1 && i!=x2) {
      inlineIncRef(t2 = exists(i,t1));
      inlineDecRef(t1);
      t1 = t2;
    }
  }

  inlineIncRef(t2 = pathReduce(t1));
  inlineDecRef(t1);

  /* Now t2 is a referenced, path reduced ddd only with x1, x2 and x0
     variables */
  mkPlotAux(s, x0, x1, x2, t2, t2);
  inlineDecRef(t2);
}

static int mkDelta(int diff, int total) {
  int factor = 1;
  int adder = 1;

  while (diff / (factor*adder) > total) {
    if (adder==1) 
      adder = 2;
    else if (adder==2)
      adder = 5;
    else {
      factor *= 10;
      adder = 1;
    }
  } /* end while */

  return factor * adder;
}

static void mkFigure(FILE* s, tVar x0, tVar x1, tVar x2, tDDD u) {
  static const char putStr[] = "\\uput[%s](%d,%d){$%s$}\n";

  int xDiff = m_xMax-m_xMin;
  int yDiff = m_yMax-m_yMin;

  assert(xDiff>=0);
  assert(yDiff>=0);

  fprintf(s,m_figureHeader);

  fprintf(s,"\\psset{xunit=%.4fmm}\n", 
	  xDiff!=0 ? ((float) m_width)/((float) xDiff) : 1);
  fprintf(s,"\\psset{yunit=%.4fmm}\n", 
	  yDiff!=0 ? ((float) m_height)/((float) yDiff) : 1);
  fprintf(s,"\\pspicture(%d,%d)(%d,%d)\n",m_xMin,m_yMin,m_xMax,m_yMax);

  if (m_showAxisInFront)
    mkPlot(s, x0, x1, x2, u);

  /* Check whether axis should be plotted */
  if (m_showAxis) {
    /* check whether BOTH axis is visible in the window: */
    if (m_xMin<=0 && m_xMax>=0 && m_yMin<=0 && m_yMax>=0) {
      fprintf(s, putStr, "r", m_xMax, 0, nameOfVar(x1));
      fprintf(s, putStr, "u", 0, m_yMax, nameOfVar(x2));

      /* the magic number 4 in the call to mkDelta ensures that max 4
         ticks pr cm (apprx.).   */
      fprintf(s, "\\psaxes[Dx=%d,Dy=%d,ticks=%s,labels=%s]%s(0,0)(%d,%d)(%d,%d)\n",
	      mkDelta(xDiff,m_width/4), mkDelta(yDiff,m_height/4),
	      m_showTicks?"all":"none",
	      m_showNumbers?"all":"none",
	      m_arrows,
	      m_xMin, m_yMin, m_xMax, m_yMax);
    }
    else {
      fprintf(s, putStr, "r", m_xMax, m_yMin, nameOfVar(x1));
      fprintf(s, putStr, "u", m_xMin, m_yMax, nameOfVar(x2));

      fprintf(s, "\\psaxes[Ox=%d,Oy=%d,Dx=%d,Dy=%d,ticks=%s,labels=%s]%s(%d,%d)(%d,%d)\n",
	      m_xMin, m_yMin,
	      mkDelta(xDiff,m_width/4), mkDelta(yDiff,m_height/4),
	      m_showTicks?"all":"none",
	      m_showNumbers?"all":"none",
	      m_arrows,
	      m_xMin, m_yMin, m_xMax, m_yMax);
    }
  }

  if (!m_showAxisInFront)
    mkPlot(s, x0, x1, x2, u);

  /* print the user-defined ps-code */
  fprintf(s,m_psCode);

  fprintf(s,"\\endpspicture\n");

  fprintf(s,m_figureFooter);
}

static void mkArticle(FILE* s, tVar x0, tVar x1, tVar x2, tDDD u) {
  fprintf(s, m_articleHeader);
  mkFigure(s, x0, x1, x2, u);
  fprintf(s, m_articleFooter);
}

void saveFigure(const char* texName, tVar x0, tVar x1, tVar x2, tDDD u) {
  FILE* texhandle;

  if ((texhandle = fopen(texName, "w"))==NULL)
    m_errorHandler(MSG_OPEN_FILE, texName);
  mkFigure(texhandle, x0, x1, x2, u);
  fclose(texhandle);
}

void saveArticle(const char* texName, tVar x0, tVar x1, tVar x2, tDDD u) {
  FILE* texhandle;

  if ((texhandle = fopen(texName, "w"))==NULL)
    m_errorHandler(MSG_OPEN_FILE, texName);
  mkArticle(texhandle, x0, x1, x2, u);
  fclose(texhandle);
}

void savePlot(const char* texName, tVar x0, tVar x1, tVar x2, tDDD u) {
  if (m_savePlotAsArticle)
    saveArticle(texName, x0, x1, x2, u);
  else
    saveFigure(texName, x0, x1, x2, u);
}

void viewPlot(tVar x0, tVar x1, tVar x2, tDDD u) {
  static char texName[] = "tmp.tex";
  static char dviName[] = "tmp.dvi";
  static char psName[] = "tmp.ps";
  char *dirName;
  char texfullname[1024];

  dirName = tmpnam(NULL);
  sprintf(texfullname, "%s/%s",dirName,texName);

  executeCmd("mkdir %s", dirName);
  saveArticle(texfullname, x0, x1, x2, u);
  executeCmd("cd %s; latex %s >/dev/null", dirName, texName);
  executeCmd("cd %s; dvips -o %s %s >&/dev/null", dirName, psName, dviName);

  /* Works with bash and tcsh (that is: sh must be a link to bash or tcsh */
  executeCmd("((cd %s; ghostview %s) ; rm -rf %s) &", dirName,psName, dirName);
}

static const char NOTSYMBOL[] = "¬"; 
static const char EQSYMBOL[]  = "=";
static const char LEQSYMBOL[] = "<=";
static const char LESYMBOL[]  = "<";
static const char GEQSYMBOL[] = ">=";
static const char GRSYMBOL[]  = ">";

#define compOfCstrOp(c) (opOfCstrOp(c)==COMP_LESS ? LE : LEQ)

typedef struct  {
  tVar p;
  tVar n;
  tComp o;
  tCstr c;
} tCstrElem;

tCstrElem* m_stack;
static unsigned int m_stackMax;
static unsigned int m_stackCur;

void pushStack(tVar p, tVars n, tComp o, tCstr c) {
  if (m_stackCur==m_stackMax)
    m_stack = reallocMem(m_stack, (m_stackMax*=2)*sizeof(tCstrElem));

  m_stack[m_stackCur].p = p;
  m_stack[m_stackCur].n = n;
  m_stack[m_stackCur].o = o;
  m_stack[m_stackCur].c = c;
  m_stackCur++;
}

void popStack(void) {
  assert(m_stackCur>0);
  m_stackCur--;
}


static tComp turnComp(tComp o) {
  switch(o) {
  case EQ:  return EQ;
  case LEQ: return GEQ;
  case GEQ: return LEQ;
  case LE:  return GR;
  case GR:  return LE;
  default:  return EQ;
  }
}

static const char* strComp(tComp o) {
  switch(o) {
  case EQ:  return EQSYMBOL;
  case LEQ: return LEQSYMBOL;
  case GEQ: return GEQSYMBOL;
  case LE:  return LESYMBOL;
  case GR:  return GRSYMBOL;
  default:  return "";
  }
}
  

static void printTerm1(tVar z, tVar p, tVar n, tComp o, tCstr u) {
  /* print p-n o u */
  if (n==z)
    printf("%s%s%d",nameOfVar(p),strComp(o),u);
  else if (p==z)
    printf("%s%s%d",nameOfVar(n),strComp(turnComp(o)),-u);
  else if (u==0)
    printf("%s%s%s",nameOfVar(p), strComp(o), nameOfVar(n));
  else if (u>0)
    printf("%s-%s%s%d",nameOfVar(p), nameOfVar(n), strComp(o),u);
  else
    printf("%s-%s%s%d",nameOfVar(n), nameOfVar(p), strComp(turnComp(o)),-u);
}

static void printTerm2(tVar z, tVar p, tVar n, tComp ol, tCstr l, tComp ou, tCstr u) {
  /* print l ol p-n ou u e.g. 5<=x-y<=6 */
  if (n==z)
    printf("%d%s%s%s%d",l,strComp(ol),nameOfVar(p),strComp(ou),u);
  else if (p==z)
    printf("%d%s%s%s%d",-u,strComp(ou),nameOfVar(n),strComp(ol),-l);
  else if (l>=0 || u>=0)
    printf("%d%s%s-%s%s%d",l,strComp(ol),nameOfVar(p),nameOfVar(n),strComp(ou),u);
  else
    printf("%d%s%s-%s%s%d",-u,strComp(ou),nameOfVar(n),nameOfVar(p),strComp(ol),-l);
}


void printStack(tVar z) {
  int i;
  for (i=0; i<m_stackCur; i++) {
    tVar p = m_stack[i].p;
    tVar n = m_stack[i].n;
    tComp o = m_stack[i].o;
    tCstr c = m_stack[i].c;

    if (typeOfVar(p)==BOOLEAN && kindOfVar(p)==USER) {
      assert(c==0 && (o==LE || o==GEQ));
      printf("%s%s ",o==LE? " ":NOTSYMBOL,nameOfVar(p));
    }
    else {
      assert(typeOfVar(p)==REAL && typeOfVar(n)==REAL);

      if (i+1<m_stackCur && m_stack[i+1].p==p && m_stack[i+1].n==n) {
	tComp no = m_stack[i+1].o;
	tCstr nc = m_stack[i+1].c;
	
	assert(o==GR || o==GEQ);
	assert(c<nc || (c==nc && o==GEQ));
	if (no==GR || no==GEQ) {
	  // skip to the next cstr
	}
	else {
	  if (c==nc && o==GEQ) {
	    assert(no==LEQ);
	    printTerm1(z,p,n,EQ,c);
	    printf(" ");
	  }
	  else {
	    printTerm2(z,p,n,o==GR?LE:LEQ,c,no,nc);
	    printf(" ");
	  }
	  i++;
	}
      }
      else {
	printTerm1(z,p,n,o,c);
	printf(" ");
      }
    }
  }
  printf("\n"); fflush(stdout);
}



static void dnf_aux(tDDD u, tVar z) {
  if (u==0) {
  }
  else if (u==1) {
    printStack(z);
  }
  else {
    pushStack(pos(u), neg(u),
	      opOfCstrOp(cstrOp(u))==COMP_LESS ? GEQ : GR,
	      valueOfCstrOp(cstrOp(u)));
    dnf_aux(low(u),z);
    popStack();

    pushStack(pos(u), neg(u),
	      opOfCstrOp(cstrOp(u))==COMP_LESS ? LE : LEQ,
	      valueOfCstrOp(cstrOp(u)));
    dnf_aux(high(u),z);
    popStack();
  }
}


void printDNF(tDDD u, tVar z) {
  PRECONDITION(isValidRefDDD(u));
  if (u==0) {
    printf("false\n");
  }
  else if (u==1) {
    printf("true\n");
  }
  else {
    m_stack = allocMem(m_stack, (m_stackMax=2)*sizeof(tCstrElem));
    m_stackCur = 0;
    dnf_aux(u,z);
    freeMem(m_stack);
  }
}


void printINFpretty(tDDD u) {
  tVar p = pos(u);
  tVar n = neg(u);
  tCstrOp c = cstrOp(u);
  if (u==0) {
    printf("false");
  }
  else if (u==1) {
    printf("true");
  }
  else if (low(u)==0 && high(u)==1) {
    if (typeOfVar(p)==BOOLEAN) {
      printf("%s", nameOfVar(p));
    }
    else {
      printTerm1((tVar)-1, p,n, compOfCstrOp(c), valueOfCstrOp(c));
    }
  }
  else if (low(u)==1 && high(u)==0) {
    if (typeOfVar(p)==BOOLEAN) {
      printf("%s%s", NOTSYMBOL,nameOfVar(p));
    }
    else {
      printTerm1((tVar)-1, n,p,
		 compOfCstrOp(negCstrOp(c)), valueOfCstrOp(negCstrOp(c)));
    }
  }
  else if (low(u)==0) {
    if (typeOfVar(p)==BOOLEAN) {
      printf("%s", nameOfVar(p));
    }
    else {
      printTerm1((tVar)-1, p,n, compOfCstrOp(c), valueOfCstrOp(c));
    }
    printf(" /\\ "); printINFpretty(high(u));
  }
  else if (high(u)==0) {
    if (typeOfVar(p)==BOOLEAN) {
      printf("%s%s", NOTSYMBOL,nameOfVar(p));
    }
    else {
      printTerm1((tVar)-1, n,p,
		 compOfCstrOp(negCstrOp(c)), valueOfCstrOp(negCstrOp(c)));
    }
    printf(" /\\ "); printINFpretty(low(u));
  }
  else if (low(u)==1) {
    if (typeOfVar(p)==BOOLEAN) {
      printf("%s%s", NOTSYMBOL,nameOfVar(p));
    }
    else {
      printTerm1((tVar)-1, p,n, compOfCstrOp(c), valueOfCstrOp(c));
    }
    printf(" \\/ "); printINFpretty(high(u));
  }
  else if (high(u)==1) {
    if (typeOfVar(p)==BOOLEAN) {
      printf("%s", nameOfVar(p));
    }
    else {
      printTerm1((tVar)-1, n,p,
		 compOfCstrOp(negCstrOp(c)), valueOfCstrOp(negCstrOp(c)));
    }
    printf(" \\/ "); printINFpretty(low(u));
  }
  else if (typeOfVar(pos(u))==BOOLEAN &&
	   low(u)>=1 && high(u)>=1 && vars(low(u))==vars(high(u)) && cstrOp(low(u))==cstrOp(high(u)) &&
	   low(low(u))==high(high(u)) &&
	   low(high(u))==0 && high(low(u))==0) {
    printf("%s == %s", nameOfVar(p),nameOfVar(pos(low(u))));
    if (low(low(u))!=1) {
      printf(" /\\ "); printINFpretty(low(low(u)));
    }
  }
  else if (typeOfVar(pos(u))==BOOLEAN &&
	   low(u)>=1 && high(u)>=1 && vars(low(u))==vars(high(u)) && cstrOp(low(u))==cstrOp(high(u)) &&
	   low(high(u))==high(low(u)) &&
	   low(low(u))==0 && high(high(u))==0) {
    printf("%s =/= %s", nameOfVar(p),nameOfVar(pos(low(u))));
    if (low(high(u))!=1) {
      printf(" /\\ "); printINFpretty(low(high(u)));
    }
  }
  else {
    if (typeOfVar(pos(u))==BOOLEAN) {
      printf("%s",nameOfVar(p));
    }
    else {
      printTerm1((tVar)-1, pos(u), neg(u),
		 compOfCstrOp(cstrOp(u)), valueOfCstrOp(cstrOp(u)));
    }
    printf(" -> "); printINFpretty(high(u)); printf(", "); printINFpretty(low(u));
  }
}

void printINF(tDDD u) {
  if (u<=1) {
    printf("%s", u==0? "F" : "T");
  }
  else {
    if (typeOfVar(pos(u))==BOOLEAN) {
      printf("%s",nameOfVar(pos(u)));
    }
    else {
      printf("%s-%s%s%d",nameOfVar(pos(u)), nameOfVar(neg(u)),
	     strComp(compOfCstrOp(cstrOp(u))),valueOfCstrOp(cstrOp(u)));
      
      /*      printTerm1((tVar)-1, pos(u), neg(u),
		 compOfCstrOp(cstrOp(u)), valueOfCstrOp(cstrOp(u)));
      */
    }
    printf(" -> "); 
    if (high(u)<=1) {
      printINF(high(u));
    }
    else {
      printf("("); 
      printINF(high(u));
      printf(")"); 
    }     
    printf(",");
    if (low(u)<=1) {
      printINF(low(u));
    }
    else {
      printf("("); 
      printINF(low(u));
      printf(")"); 
    }     
  }
}

void save_to_file_aux(FILE* output, tDDD u) {
  if (u<=1) {
    fprintf(output, "0,0,%d,%d", u,u);
  }
  else {
    fprintf(output, "%u,%d,", vars(u), cstrOp(u));
    save_to_file_aux(output, high(u));
    fprintf(output, ",");
    save_to_file_aux(output, low(u));
  }
}

void save_to_file(const char* name, tDDD u) {
  FILE* output;
  if ((output = fopen(name, "w"))==NULL)
    m_errorHandler(MSG_OPEN_FILE, name);
  save_to_file_aux(output, u);
  fclose(output);
}

tDDD restore_from_file_aux(FILE* input) {
  tVars v;
  tCstrOp c;
  tDDD h,l,r;

  if (fscanf(input, "%u,%d,", &v, &c)==2) {
    if (v==0 && c==0) {
      if (fscanf(input, "%d,%d", &h, &l)==2 && l==h && (l==0 || l==1)) {
	return l;
      }
      else {
	printf("Error in input file: not a proper terminal node\n");
	exit(1);
      }
    }
    else {
      inlineIncRef(h = restore_from_file_aux(input));
      fscanf(input, ",");
      inlineIncRef(l = restore_from_file_aux(input));
      r = mk(v,c,h,l); inlineDecRef(h); inlineDecRef(l);
      return r;
    }
  }
  else {
    printf("Error in input file: not a proper variable or constraint\n");
    exit(1);
  }
}

tDDD restore_from_file(const char* name) {
  FILE* input;
  tDDD r;
  if ((input = fopen(name, "r"))==NULL)
    m_errorHandler(MSG_OPEN_FILE, name);
  r = restore_from_file_aux(input);
  fclose(input);
  return r;
}

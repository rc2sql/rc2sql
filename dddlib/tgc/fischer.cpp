#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <tgc.h>

static int max = 0;

int N; // the number of protocols

int search_direction;
#define BACKWARD 0
#define FORWARD  1
const char* search_direction_string[2] = { "backward", "forward" };

int reduction_type;
#define NO_REDUCTION        0
#define PATH_REDUCTION      1
#define MERGE_REDUCTION 2
const char* reduction_type_string[3] = { "no", "path", "merge/path" };

bool matches(char* str, char* pattern) {
  return !strncasecmp(pattern, str, strlen(pattern));
}


int get_options(int argc, char* argv[]) {
  int i = 1;
  while (i<argc-2) {
    if (matches(argv[i], "-s")) {
      if (matches(argv[i+1], "f"))
	search_direction = FORWARD;
      else if (matches(argv[i+1], "b"))
	search_direction = BACKWARD;
      else {
	printf("Unknown -search option: %s\n", argv[i+1]);
	return 0;
      }
      i += 2;
    }
    else if (matches(argv[i], "-r")) {
      if (matches(argv[i+1], "n"))
	reduction_type = NO_REDUCTION;
      else if (matches(argv[i+1], "p"))
	reduction_type = PATH_REDUCTION;
      else if (matches(argv[i+1], "m"))
	reduction_type = MERGE_REDUCTION;
      else {
	printf("Unknown -reduction option: %s\n", argv[i+1]);
	return 0;
      }
      i += 2;
    }
    else {
      printf("Unknown option: %s\n", argv[i]);
      return 0;
    }
  }
  return 1;
}

void progress_func(unsigned int i, const ddd& R) {
  int nc = NodeCount(R);
  double pc = PathCount(R);
  max = nc>max ? nc : max;
  printf("[%d:%d,%g] ", i, nc,pc);
  fflush(stdout);
}

ddd reduction_func(const ddd& R) {
  switch (reduction_type) {
  case 1: return PathReduce(R);
  case 2: return PathReduce(Merge(R));
  default: return R;
  }
}

boolean *a, *ap, *b, *bp;
real u, z, zp, zpp, *x, *xp;    // the nullpoint and the timer of each protocol
boolean *id, *idp;          // should really be an integer.
const int k = 10;

void init() {
  ddd::Init(5*N+4, 64);
  a   = (boolean*) calloc(N, sizeof(boolean));
  ap  = (boolean*) calloc(N, sizeof(boolean));
  b   = (boolean*) calloc(N, sizeof(boolean));
  bp  = (boolean*) calloc(N, sizeof(boolean));
  id  = (boolean*) calloc(N, sizeof(boolean));
  idp = (boolean*) calloc(N, sizeof(boolean));
  x   = (real*)    calloc(N, sizeof(real));
  xp  = (real*)    calloc(N, sizeof(real));

  int i;
  for (i=0; i<N; i++) { 
    a[i] = boolean("a[%d]", i);
    ap[i] = boolean("ap[%d]", i);
    b[i] = boolean("b[%d]", i);
    bp[i] = boolean("bp[%d]", i);
    id[i]  = boolean("id[%d]", i);
    idp[i] = boolean("idp[%d]", i);
    x[i]   = real("x[%d]",  i);
    xp[i]  = real("xp[%d]",  i);
  }
  z   = real("z");
  zp  = real("zp");
  zpp  = real("zpp");

  ddd::ClearOrder();
  for (i=0; i<N; i++) {
    ddd::AppendToOrder(a[i]);    ddd::AppendToOrder(ap[i]);
    ddd::AppendToOrder(b[i]);    ddd::AppendToOrder(bp[i]);
    ddd::AppendToOrder(id[i]);    ddd::AppendToOrder(idp[i]);
    ddd::AppendToOrder(x[i], z);    ddd::AppendToOrder(xp[i], z);
    ddd::AppendToOrder(x[i], zp);    ddd::AppendToOrder(xp[i], zp);
    ddd::AppendToOrder(x[i], zpp);    ddd::AppendToOrder(xp[i], zpp);
    for (int j=0; j<N; j++)
      if (i!=j) {
	ddd::AppendToOrder(x[i], x[j]);
	ddd::AppendToOrder(xp[i], x[j]);
	ddd::AppendToOrder(x[i], xp[j]);
	ddd::AppendToOrder(xp[i], xp[j]);
      }
  }
  ddd::OrderVariables(APPEND_INC_LEX);
  ddd::AppendToOrder(z,zp);
  ddd::AppendToOrder(z,zpp);
  ddd::AppendToOrder(zp,zpp);

}

#define idle(i)  (!a[i]&!b[i])
#define idlep(i) (!ap[i]&!bp[i])
#define rdy(i)   (!a[i]&b[i])
#define rdyp(i)  (!ap[i]&bp[i])
#define wait(i)  (a[i]&!b[i])
#define waitp(i) (ap[i]&!bp[i])
#define crit(i)  (a[i]&b[i])
#define critp(i) (ap[i]&bp[i])

ddd id_is(int j) {
  int i;
  ddd R = True;
  for (i=0; i<N; i++) {
    R &= j==i ? id[i] : !id[i];
  }
  return R;
}


int main(int argc, char* argv[]) {
  int i = 1;

  if (!get_options(argc,argv)) {
    printf("Usage: %s -s(earch) <dir> -r(eduction) <red> <#processes> \n\n"\
	   "<dir>: b(ackward), f(orward)\n"\
	   "<red>: n(o), p(ath), m(erge)\n",
	   argv[0]);
    exit(1);
  }
  N =  argc>1 ? atoi(argv[argc-1]) : 1;

  printf("Processes: %d\n"\
	 "Search:    %s\n"\
	 "Reduction: %s\n",
	 N,
	 search_direction_string[search_direction],
	 reduction_type_string[reduction_type]);

  init();

  tgc fischer;
  fischer.set_progress_func(progress_func);
  fischer.set_reduction_func(reduction_func);

  ddd inv = True;
  for (i=0; i<N; i++) {
    inv &= Forall(zpp, (zp<=zpp & zpp<=z) >> ((rdy(i)) >> (x[i]-zpp>=0 & x[i]-zpp<=k)));
  }

  for (i=0; i<N; i++) {
    // The initial state
    fischer.add_initial(idle(i) & !id[i] & x[i]==z);

    // (IDLE(i)\/WAIT(i)) /\ id==0  -->  s,x := RDY,0
    fischer.add_cmd(varlist().add(x[i]).add(a[i]).add(b[i]),
		    varlist().add(xp[i]).add(ap[i]).add(bp[i]),
		    (idle(i) | wait(i)) & id_is(-1) & rdyp(i) & xp[i]==z);

    // RDY(i) /\ x<=k  -->  s,x,id := WAIT,0,i
    fischer.add_cmd(varlist().add(x[i]).add(a[i]).add(b[i]).add(id[i]),
		    varlist().add(xp[i]).add(ap[i]).add(bp[i]).add(idp[i]),
		    rdy(i) & x[i]-z<=k & waitp(i) & xp[i]==z & idp[i]);

    // WAIT(i) /\ x>k /\ id==i  -->  s := CRIT (evt. x:=?)
    fischer.add_cmd(varlist().add(a[i]).add(b[i]),
		    varlist().add(ap[i]).add(bp[i]),
		    wait(i) & x[i]-z>k & id_is(i) & critp(i));

    // CRIT -->  s,id := IDLE,0 (evt. x:=?)
    fischer.add_cmd(varlist().add(a[i]).add(b[i]).add(id[i]),
		    varlist().add(ap[i]).add(bp[i]).add(idp[i]),
		    crit(i) & idlep(i) & !idp[i]);
  }

  fischer.add_cmd(varlist().add(z),
		  varlist().add(zp),
		  zp<=z & inv);

  ddd mutex = True;
  for (i=0; i<N; i++)
    for (int j=0; j<N; j++)
      if (i!=j) mutex &= !(crit(i) & crit(j));
  
  bool satisfied = false;
  ddd R;
  switch (search_direction) {
  case BACKWARD:
    R = fischer.prestar(!mutex);
    satisfied = Unsatisfiable(R & fischer.m_init);
    break;
  case FORWARD:
    R = fischer.poststar(fischer.m_init);
    satisfied = Tautology(R >> mutex);
    break;
  default:
    break;
  }
  printf("\n"\
	 "Property : %ssatisfied\n"\
	 "Rmax     : %d\n"\
	 "Rfinal   : %d\n",
	 satisfied ? "" : "NOT ",
	 max,
	 NodeCount(R));
}

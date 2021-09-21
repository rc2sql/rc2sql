#include <stdio.h>
#include <stdlib.h>
#include <dddcpp.h>
#include <time.h>

class Timer {
public:
  void start() { m_clock = clock(); }
  void stop()  { m_clock = clock() - m_clock; }
  long msec()  { return (1000*m_clock)/CLOCKS_PER_SEC; }
  void show()  { printf("Elapsed time = %lu\n", msec()); }
private:
  clock_t m_clock;
};

static int N;
tCstr *TL, *TU, HL, HU;
boolean *c, *h, *t;
real ZPP, ZP, Z, H, *T;

void init() {
  int i;

  ddd::Init(N*10, 64);
  /* Variables */
  c = (boolean*) calloc(N, sizeof(boolean));
  h = (boolean*) calloc(N, sizeof(boolean));
  t = (boolean*) calloc(N, sizeof(boolean));
  T = (real*) calloc(N, sizeof(real));

  /* Taks bounds */
  TL = (tCstr*) calloc(N, sizeof(tCstr));
  TU = (tCstr*) calloc(N, sizeof(tCstr));

  for (i=0; i<N; i++) { 
    t[i] = boolean("t%d",  i+1);
    c[i] = boolean("c%d",  i+1);
    h[i] = boolean("h%d",  i+1);
  }
  HL = 25;
  HU = 200;

  Z = real("Z");
  ZP = real("Z'");
  ZPP = real("Z''");
  H = real("H");
  for (i=0; i<N; i++) {
    T[i] = real("T%d",  i+1);
    TL[i] = 80;
    TU[i] = 100;
  }

  ddd::ClearOrder();

  for (i=0; i<N; i++) { 
    ddd::AppendToOrder(T[i],H);
  }
  ddd::PrependToOrder(ZP,ZPP);
  ddd::PrependToOrder(Z,ZPP);
  ddd::PrependToOrder(Z,ZP);
  ddd::PrependToOrder(H,ZPP);
  ddd::PrependToOrder(H,ZP);
  ddd::PrependToOrder(H,Z);
  for (i=N-1; i>=0; i--) { 
    ddd::PrependToOrder(T[i],ZPP);
    ddd::PrependToOrder(T[i],ZP);
    ddd::PrependToOrder(T[i],Z);
  }
  for (i=N-1; i>=0; i--) { 
    ddd::PrependToOrder(t[i]);
    ddd::PrependToOrder(c[i]);
    ddd::PrependToOrder(h[i]);
  }
  ddd::OrderVariables(APPEND_INC_LEX);

  //  ddd::PrintOrder();
}


ddd initial() {
  ddd I = c[0] & !t[0] & !h[0];
  for (int i=1; i<N; i++)
    I &= !c[i] & !t[i] & !h[i];
  return I;
}


ddd nonurg(const ddd& R) {
  ddd U = R;
  for (int i=0; i<N; i++)
    U &= !(c[i] & !t[i]);
  return U;
}


ddd stateinv(const ddd& R) {
  ddd W = R;
  for (int i=0; i<N; i++)
    W &= (t[i] >> (T[i]-Z<=TU[i]))
       & (h[i] >> (H-Z<=HU));
  return W;
}


ddd inv(const ddd& R) {
  ddd W = R;
  for (int i=0; i<N; i++)
    W &= (t[i] >> (T[i]-ZP<=TU[i]))       // t_i => (T_i - z' <= T^u)
      &  (h[i] >> (H-ZP<=HU))             // h_i => (H   - z' <= H^u)
      &  Forall(ZPP, (ZP<ZPP & ZPP<=Z)    // \-/ z'' (z' < z'' <= z) => 
	        >> ((t[i] >> (T[i]-ZPP<=TU[i])) & (h[i] >> (H-ZPP<=HU))));
  return W;
}

ddd advTime(const ddd& R) {
  ddd advR = stateinv(Replace(Exists(Z, inv(nonurg(R) & ZP<=Z)), Z,ZP));
  return advR;
}


ddd next(const ddd& Ri) {
  ddd R = False;
  for (int i=0; i<N; i++) {
    // The cycler grabs the token and starts its task
    R |= stateinv(Assign(Assign(Assign(Assign(Assign(Ri & c[i] & !t[i], t[i],1),
				     c[i],0), h[i],1), T[i],Z,0), H,Z,0));

    // The cycler gives the token to the next cycler
    R |= stateinv(Assign(Assign(Ri & h[i] & HL<=H-Z, c[(i+1)%N],1), h[i],0));

    // Stop the task within the lower and upper bounds
    R |= stateinv(Exists(T[i], Assign(Ri & t[i] & TL[i]<=T[i]-Z, t[i],0)));
  }
  return R;
}


ddd ReachFrontset(void) {
  ddd R = initial();
  ddd F = R; ddd nF, tF;
  time_t tstart = time(NULL);
  printf("#%s%8s%8s%8s%6s\n", "iter", "Space", "Paths", "Front", "Time");
  int i=0;
  ddd RP;
  while (Satisfiable(F)) {
    nF = next(F);
    tF = advTime(F);
    F = PathReduce((nF|tF)-R);
    R = PathReduce(R|F);
    printf(" %2d%8d%8g%8d%6lu\n", ++i, NodeCount(R), PathCount(R),
    	   NodeCount(F), time(NULL)-tstart); fflush(stdout);
  }
  return R;
}

ddd Reach(void) {
  ddd R = initial();
  ddd RP;
  int i=0;
  time_t tstart = time(NULL);
  do {
    RP = R;
    R = PathReduce(R | next(R) | advTime(R));
    i++;
  }
  while (!Equivalent(R,RP));
  printf("%d & %d & %lu & %d\\\\\n",N, NodeCount(R),time(NULL)-tstart,i);
  return R;
}

ddd NewReach(void) {
  ddd R = initial();
  ddd RP;
  time_t tstart = time(NULL);
  printf("#%s%8s%8s%6s\n", "iter", "Space", "Paths", "Time");
  int i=0;
  do {
    RP = R;
    R = (R | advTime(next(R)));
    printf(" %2d%8d%8g%6lu\n", ++i, NodeCount(R), PathCount(R),
           time(NULL)-tstart);
    fflush(stdout);
  }
  while (!Equivalent(R,RP));
  return R;
}

/// ----------------------

int main(int argc, char* argv[]) {
  N = argc>1 ? atoi(argv[1]) : 1;
  printf("Computing the reachable state space for N=%d cyclers.\n",N);

  init();

  Timer timer;
  timer.start();
  timer.stop();

  ddd R1 = ReachFrontset();
  //  SaveDDD("milner_sch.ddd", R1);

  return 0;
}

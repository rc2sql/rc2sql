#include <stdio.h>
#include <stdlib.h>
#include <tgc.h>

void progress_func(unsigned int i, const ddd& R) {
  int nc = NodeCount(R);
  double pc = PathCount(R);
  printf("[%d:%d,%g] ", i, nc,pc);
  fflush(stdout);
}

ddd reduction_func(const ddd& R) {
  return R;
}

static int N;

tCstr *TL, *TU, HL, HU;
boolean *c, *cp, *h, *hp, *t, *tp;
real ZPP, ZP, Z, H, HP, *T, *TP;

void init() {
  int i;

  ddd::Init(N*16 + 5, 64); // 64 MB of memory for the DDD package

  /* Taks bounds */
  HL = 25;
  HU = 200;
  TL = (tCstr*) calloc(N, sizeof(tCstr));
  TU = (tCstr*) calloc(N, sizeof(tCstr));
  for (i=0; i<N; i++) {
    TL[i] = 80;
    TU[i] = 100;
  }

  /* Variables */
  c  = (boolean*) calloc(N, sizeof(boolean));
  cp = (boolean*) calloc(N, sizeof(boolean));
  h  = (boolean*) calloc(N, sizeof(boolean));
  hp = (boolean*) calloc(N, sizeof(boolean));
  t  = (boolean*) calloc(N, sizeof(boolean));
  tp = (boolean*) calloc(N, sizeof(boolean));
  T  = (real*) calloc(N, sizeof(real));
  TP = (real*) calloc(N, sizeof(real));

  Z = real("Z");
  ZP = real("Z'");
  ZPP = real("Z''");
  H = real("H");
  HP = real("H'");
  for (i=0; i<N; i++) { 
    t[i]  = boolean("t%d",  i+1);
    tp[i] = boolean("t%d",  i+1);
    c[i]  = boolean("c%d",  i+1);
    cp[i] = boolean("c%d",  i+1);
    h[i]  = boolean("h%d",  i+1);
    hp[i] = boolean("h%d",  i+1);
    T[i]  = real("T%d",  i+1);
    TP[i] = real("T'%d",  i+1);
  }

  ddd::ClearOrder();
  // Order the variables
  for (i=0; i<N; i++) { 
    ddd::AppendToOrder(h[i]);
    ddd::AppendToOrder(hp[i]);
    ddd::AppendToOrder(c[i]);
    ddd::AppendToOrder(cp[i]);
    ddd::AppendToOrder(t[i]);
    ddd::AppendToOrder(tp[i]);
  }
  for (i=0; i<N; i++) { 
    ddd::AppendToOrder(T[i],Z);
    ddd::AppendToOrder(TP[i],Z);
    ddd::AppendToOrder(T[i],ZP);
    ddd::AppendToOrder(TP[i],ZP);
    ddd::AppendToOrder(T[i],ZPP);
    ddd::AppendToOrder(TP[i],ZPP);
  }
  ddd::AppendToOrder(H,ZPP);
  ddd::AppendToOrder(H,ZP);
  ddd::AppendToOrder(H,Z);

  ddd::AppendToOrder(ZP,ZPP);
  ddd::AppendToOrder(Z,ZPP);
  ddd::AppendToOrder(Z,ZP);
  for (i=0; i<N; i++) { 
    ddd::AppendToOrder(T[i],H);
    ddd::AppendToOrder(TP[i],H);
    ddd::AppendToOrder(T[i],HP);
    ddd::AppendToOrder(TP[i],HP);
    /*    for (int j=0; j<N; j++)
      if (i!=j) {
	ddd::AppendToOrder(T[i], T[j]);
	ddd::AppendToOrder(TP[i], T[j]);
	ddd::AppendToOrder(T[i], TP[j]);
	ddd::AppendToOrder(TP[i], TP[j]);
	}*/
  }
  ddd::OrderVariables(APPEND_INC_LEX);
}

int main(int argc, char* argv[]) {
  int i = 1;

  N =  argc>1 ? atoi(argv[argc-1]) : 1;

  init();

  tgc milner;
  milner.set_progress_func(progress_func);
  milner.set_reduction_func(reduction_func);

  ddd inv = True;
  for (i=0; i<N; i++) {
    inv &= Forall(ZPP, (ZP<=ZPP & ZPP<=Z) >> (t[i] >> (T[i]-ZPP>=0 & T[i]-ZPP<=TU[i])));
    inv &= Forall(ZPP, (ZP<=ZPP & ZPP<=Z) >> (h[i] >> (H-ZPP>=0 & H-ZPP<=HU)));
    inv &= Forall(ZPP, (ZP<ZPP & ZPP<=Z) >> !(c[i] & !t[i])); // First command is urgent
  }

  milner.add_initial(c[0] & !t[0] & !h[0]);
  for (i=1; i<N; i++)
    milner.add_initial(!c[i] & !t[i] & !h[i]);

  for (i=0; i<N; i++) {
    // The cycler grabs the token and starts its task
    milner.add_cmd(varlist().add(t[i]).add(c[i]).add(h[i]).add(T[i]).add(H),
		   varlist().add(tp[i]).add(cp[i]).add(hp[i]).add(TP[i]).add(HP),
		   c[i] & !t[i] & 
		   tp[i] & !cp[i] & hp[i] & TP[i]-Z==0 & HP-Z==0);

    // The cycler gives the token to the next cycler
    milner.add_cmd(varlist().add(c[(i+1)%N]).add(h[i]),
		   varlist().add(cp[(i+1)%N]).add(hp[i]),
		   h[i] & HL<=H-Z &  
		   cp[(i+1)%N] & !hp[i]);

    // Stop the task within the lower and upper bounds
    milner.add_cmd(varlist().add(t[i]).add(T[i]),
		   varlist().add(tp[i]).add(TP[i]),
		   t[i] & TL[i]<=T[i]-Z &
		   !tp[i]);
  }

  
  milner.add_cmd(varlist().add(Z),
		 varlist().add(ZP),
		 ZP<=Z & inv);


  printf("Computing reachable state space for Milner's Scheduler with N=%d "
	 "using frontsets\n",N);
  ddd reach = milner.reachable_state_space_frontset();
}



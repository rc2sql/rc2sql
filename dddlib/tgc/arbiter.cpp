#include <stdio.h>
#include <stdlib.h>
#include <tgcl.h>
#include <time.h>

// The minimum pulse widths
tCstr Rh[2], Rl[2], Gh[2], Gl[2], Dh, Dl;

// Client and arbiter state
enumerator c[2], a[2];

#define IDLE(x)  (x==0)
#define REQ(x)   (x==1)
#define GRANT(x) (x==3)
#define PEND(x)  (x==2)

// pulse signals
boolean r[2], g[2], d;

// arbiter has acquired request/done, client has acquired grant.
// f indicates whether the privilege is free
boolean racq[2], gacq[2], dacq, f;

// Pulse timers
real ZPP, ZP, Z, R[2], G[2], D;

void init() {
  int i;
  ddd::Init(64, 32);

  f = boolean("f");
  d = boolean("d");
  dacq = boolean("dacq");
  D = real("D");
  Z = real("Z");
  ZP = real("ZP");
  ZPP = real("ZPP");
  for (i=0; i<2; i++) {
    c[i] = enumerator(4, "c%d",i);
    a[i] = enumerator(4, "a%d",i);
    r[i] = boolean("r[%d]",i);
    g[i] = boolean("g[%d]",i);
    racq[i] = boolean("racq[%d]",i);
    gacq[i] = boolean("gacq[%d]",i);
    R[i] = real("R[%d]",i);
    G[i] = real("G[%d]",i);
  }

  ddd::ClearOrder();


  ddd::AppendToOrder(g[0]);
  ddd::AppendToOrder(gacq[0]);
  ddd::AppendToOrder(r[0]);
  ddd::AppendToOrder(racq[0]);
  ddd::AppendToOrder(c[0]);
  ddd::AppendToOrder(a[0]);

  ddd::AppendToOrder(d);
  ddd::AppendToOrder(dacq);
  ddd::AppendToOrder(f);
  
  ddd::AppendToOrder(g[1]);
  ddd::AppendToOrder(gacq[1]);
  ddd::AppendToOrder(r[1]);
  ddd::AppendToOrder(racq[1]);
  ddd::AppendToOrder(c[1]);
  ddd::AppendToOrder(a[1]);

  ddd::AppendToOrder(R[0],Z);
  ddd::AppendToOrder(R[0],ZP);
  ddd::AppendToOrder(R[0],ZPP);
  ddd::AppendToOrder(R[1],Z);
  ddd::AppendToOrder(R[1],ZP);
  ddd::AppendToOrder(R[1],ZPP);

  ddd::AppendToOrder(G[0],Z);
  ddd::AppendToOrder(G[0],ZP);
  ddd::AppendToOrder(G[0],ZPP);
  ddd::AppendToOrder(G[1],Z);
  ddd::AppendToOrder(G[1],ZP);
  ddd::AppendToOrder(G[1],ZPP);

  ddd::AppendToOrder(D,Z);
  ddd::AppendToOrder(D,ZP);
  ddd::AppendToOrder(D,ZPP);

  ddd::AppendToOrder(Z,ZP);
  ddd::AppendToOrder(ZP,ZPP);
  ddd::AppendToOrder(Z,ZPP);


  ddd::OrderVariables(APPEND_INC_LEX);

  Rl[0] = 3;  Rh[0] = 10007;
  Gl[0] = 1;  Gh[0] = 100031; 
  Rl[1] = 3;  Rh[1] = 119;
  Gl[1] = 11;  Gh[1] = 100042; 
  Dl = 2;     Dh = 10006;
}




int main(int argc, char* argv[]) {
  init();
  int i;

  tgc arb(Z,ZP,ZPP);

  arb.add_initial(!d & !dacq & !f & D-Z==0);
  for (i=0; i<2; i++) {
    arb.add_initial(!r[i] & !racq[i] & !g[i] & !gacq[i]);
    arb.add_initial(IDLE(c[i]) & IDLE(a[i]) & R[i]-Z==0 & G[i]-Z==0);
  }

  for (i=0; i<2; i++) {
    // c_i=idle, r_i=0, R_i>=Rl    --> c_i:=req, r_i:=1, R_i:=0
    arb.add_cmd(IDLE(c[i]) & !r[i] & R[i]-Z>=Rl[i],
		REQ(c[i]) & r[i] & R[i]-Z==0,
		varlist().add(c[i]).add(R[i]).add(r[i]));

    // c_i=grant, r_i=0, R_i>=Rl    --> c_i:=pend, r_i:=1, R_i:=0
    arb.add_cmd(GRANT(c[i]) & !r[i] & R[i]-Z>=Rl[i],
		PEND(c[i]) & r[i] & R[i]-Z==0,
		varlist().add(c[i]).add(R[i]).add(r[i]));

    // a_i=idle, r_i=1, r_i^acq=0  --> a_i:=req, r_i^acq:=1
    arb.add_cmd(IDLE(a[i]) & r[i] & !racq[i],
		REQ(a[i]) & racq[i],
		varlist().add(a[i]).add(racq[i]));

    // a_i=grant, r_i=1, r_i^acq=0 --> a_i:=pend, r_i^acq:=1
    arb.add_cmd(GRANT(a[i]) & r[i] & !racq[i],
		PEND(a[i]) & racq[i],
		varlist().add(a[i]).add(racq[i]));

    // a_i=req, g_i=0, G_i>=Gl, f=0 --> a_i:=grant, g_i:=1, G_i:=0, f:=1
    arb.add_cmd(REQ(a[i]) & !g[i] & G[i]-Z>=Gl[i] & !f,
		GRANT(a[i]) & g[i] & G[i]-Z==0 & f,
		varlist().add(G[i]).add(f).add(g[i]).add(a[i]));

    // g_i=1, G_i>=Gh              -->  g_i:=0, g_i^acq:=0, G_i:=0
    arb.add_cmd(g[i] & G[i]-Z>=Gh[i],
		!g[i] & !gacq[i] & G[i]-Z==0,
		varlist().add(G[i]).add(gacq[i]).add(g[i]));

    // c_i=req, g_i=1, g_i^acq=0   --> c_i:=grant, g_i^acq:=1
    arb.add_cmd(REQ(c[i]) & g[i] & !gacq[i],
		GRANT(c[i]) & gacq[i],
		varlist().add(c[i]).add(gacq[i]));

    // c_i=grant, d=0, D>=Dl       --> c_i:=idle, d:=1, D:=0
    arb.add_cmd(GRANT(c[i]) & !d & D-Z>=Dl,
		IDLE(c[i]) & d & D-Z==0,
		varlist().add(D).add(c[i]).add(d));

    // c_i=pend, d=0, D>=Dl        --> c_i:=req, d:=1, D:=0
    arb.add_cmd(PEND(c[i]) & !d & D-Z>=Dl,
		REQ(c[i]) & d & D-Z==0,
		varlist().add(D).add(c[i]).add(d));

    // r_i=1, R_i>=Rh              -->  r_i:=0, r_i^acq:=0, R_i:=0
    arb.add_cmd(r[i] & R[i]-Z>=Rh[i],
		!r[i] & !racq[i] & R[i]-Z==0,
		varlist().add(R[i]).add(racq[i]).add(r[i]));

    // a_i=grant, d=1, d^acq=0     --> a_i:=idle, d^acq:=1, f:=0
    arb.add_cmd(GRANT(a[i]) & d & !dacq,
		IDLE(a[i]) & dacq & !f,
		varlist().add(a[i]).add(dacq).add(f));

    // a_i=pend, d=1, d^acq=0      --> a_i:=req, d^acq:=1, f:=0
    arb.add_cmd(PEND(a[i]) & d & !dacq,
		REQ(a[i]) & dacq & !f,
		varlist().add(a[i]).add(dacq).add(f));
    
    arb.add_invariant( (IDLE(a[i]) & r[i]) >> (R[i]-Z<=Rh[i]) );
    arb.add_invariant( (GRANT(a[i]) & r[i]) >> (R[i]-Z<=Rh[i]) );
    arb.add_invariant( (REQ(c[i]) & g[i]) >> (G[i]-Z<=Gh[i]) );
    
}

  // d=1, D>=Dh                  -->  d:=0, d^acq:=0, D:=0
  arb.add_cmd(d & D-Z>=Dh,
	      !d & !dacq & D-Z==0,
	      varlist().add(D).add(dacq).add(d));

  
  printf("Checking mutual exclusion\n");
  printf("\nProperty%s satisfied\n", 
	 arb.check_property( !((GRANT(c[0])|PEND(c[0])) & (GRANT(c[1])|PEND(c[1]))) ) ? "":" NOT");
  return 0;
}

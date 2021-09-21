#include <stdio.h>
#include <stdlib.h>
#include <tgc.h>

#define out 0
#define ack 1
#define req 2
const char* names[] = { "out", "ack", "req" };

boolean* o;
boolean* op;
boolean* u;
boolean* up;
real*    x;
real*    xp;
real*    z;
real*    zp;
real*    zpp;

ddd hazard;
ddd* inv;


void add_gate_old(tgc& P, unsigned int i, const ddd& upguard, const ddd& dnguard,
	      unsigned int Tmin, unsigned int Tmax) {
  inv[0] &= Forall(zpp[0], (zp[0]<zpp[0] & zpp[0]<=z[0]) >> 
		   !((!o[i] & !u[i] & upguard) | (o[i] & !u[i] & dnguard)));
  inv[0] &= Forall(zpp[0], (zp[0]<=zpp[0] & zpp[0]<=z[0]) >> 
		   (u[i] >> (x[i]-zpp[0]>=0 & x[i]-zpp[0]<=Tmax)));

  P.add_cmd(varlist().add(u[i]).add(x[i]),
	    varlist().add(up[i]).add(xp[i]),
	    ((!o[i] & !u[i] & upguard) | (o[i] & !u[i] & dnguard)) & 
	    up[i] & xp[i]-z[0]==0);

  P.add_cmd(varlist().add(u[i]).add(o[i]).add(x[i]),
	    varlist().add(up[i]).add(op[i]).add(xp[i]),
	    u[i] & x[i]-z[0]>=Tmin & !up[i] & (o[i]!=op[i]));

  hazard |= (u[i] & !o[i] & !upguard) | (u[i] & o[i] & !dnguard);
}

void add_gate(tgc& P, unsigned int i, const ddd& upguard, const ddd& dnguard,
	      unsigned int Tmin, unsigned int Tmax) {
  inv[i] &= Forall(zpp[i], (zp[i]<zpp[i] & zpp[i]<=z[i]) >> 
		   !((!o[i] & !u[i] & upguard) | (o[i] & !u[i] & dnguard)));
  inv[i] &= Forall(zpp[i], (zp[i]<=zpp[i] & zpp[i]<=z[i]) >> 
		   (u[i] >> (x[i]-zpp[i]>=0 & x[i]-zpp[i]<=Tmax)));

  P.add_cmd(varlist().add(u[i]).add(x[i]),
	    varlist().add(up[i]).add(xp[i]),
	    ((!o[i] & !u[i] & upguard) | (o[i] & !u[i] & dnguard)) & 
	    up[i] & xp[i]-z[i]==0);

  P.add_cmd(varlist().add(u[i]).add(o[i]).add(x[i]),
	    varlist().add(up[i]).add(op[i]).add(xp[i]),
	    u[i] & x[i]-z[i]>=Tmin & !up[i] & (o[i]!=op[i]));

  hazard |= (u[i] & !o[i] & !upguard) | (u[i] & o[i] & !dnguard);
}


int main(int argc, char* argv[]) {
  int i;
  int N = sizeof(names)/sizeof(char*);
  ddd::Init(6*N+3, 64);

  o  = (boolean*)   calloc(N, sizeof(boolean));
  op = (boolean*)   calloc(N, sizeof(boolean));
  u  = (boolean*)   calloc(N, sizeof(boolean));
  up = (boolean*)   calloc(N, sizeof(boolean));
  x  = (real*)      calloc(N, sizeof(real));
  xp = (real*)      calloc(N, sizeof(real));
  z  = (real*)      calloc(N, sizeof(real));
  zp = (real*)      calloc(N, sizeof(real));
  zpp= (real*)      calloc(N, sizeof(real));
  inv = (ddd*)      calloc(N, sizeof(ddd)); 

  for (i=0; i<N; i++) {
    o[i] = boolean("%s", names[i]);
    op[i] = boolean("%s'", names[i]);
    u[i] = boolean("%s_u", names[i]);
    up[i] = boolean("%s_u'", names[i]);
    x[i] = real("%s_t", names[i]);
    xp[i] = real("%s_t'", names[i]);
    z[i] = real("%s_z", names[i]);
    zp[i] = real("%s_z'", names[i]);
    zpp[i] = real("%s_z''", names[i]);
  }

  ddd::ClearOrder();
  for (i=0; i<N; i++) {
    ddd::AppendToOrder(o[i]);      ddd::AppendToOrder(op[i]);
    ddd::AppendToOrder(u[i]);      ddd::AppendToOrder(up[i]);
    ddd::AppendToOrder(x[i], z[i]);   ddd::AppendToOrder(xp[i], z[i]);
    ddd::AppendToOrder(x[i], zp[i]);  ddd::AppendToOrder(xp[i], zp[i]);
    ddd::AppendToOrder(x[i], zpp[i]); ddd::AppendToOrder(xp[i], zpp[i]);
    ddd::AppendToOrder(z[i],zp[i]);
    ddd::AppendToOrder(z[i],zpp[i]);
    ddd::AppendToOrder(zp[i],zpp[i]);
    for (int j=0; j<N; j++)
      if (i!=j) {
	ddd::AppendToOrder(x[i], x[j]);
	ddd::AppendToOrder(xp[i], x[j]);
	ddd::AppendToOrder(x[i], xp[j]);
	ddd::AppendToOrder(xp[i], xp[j]);
      }
  }
  ddd::OrderVariables(APPEND_INC_LEX);

  int Tmin=2;
  int Tmax=5;

  if (argc>=3) {
    Tmin = atoi(argv[1]);
    Tmax = atoi(argv[2]);
  }

  tgc seitz;
  hazard = False;
  ddd I = True;
  for (i=0; i<N; i++) {
    inv[i] = True;
    I &= !u[i] & !o[i];
  }
  seitz.add_initial(I);
  add_gate(seitz, req, !o[ack],         o[ack],          3, 3);
  add_gate(seitz, out, o[ack]!=o[req],  o[ack]==o[req],  1, 2);
  add_gate(seitz, ack, o[out] & o[req], o[out]& !o[req], 1, 3);

  varlist v,vp;
  ddd expr = True;
  for (i=0; i<N; i++) {
    v.add(z[i]);
    vp.add(zp[i]);
    expr &= zp[i]<=z[i] & inv[i];
  }
  seitz.add_cmd(v,vp,expr);

  printf("Local time\n");
  ddd R = seitz.poststar(I);
  R = Replace(Replace(R, z[0],z[1]),z[0],z[2]);
  printf("R => !hazard: %d\n", Consequence(R,!hazard));

  ddd B = seitz.prestar(hazard);
  B = Replace(Replace(B, z[0],z[1]),z[0],z[2]);
  printf("|/= B /\\ I: %d\n", Unsatisfiable(B & I));

  tgc seitz_old;
  hazard = False;
  I = True;
  for (i=0; i<N; i++) {
    inv[i] = True;
    I &= !u[i] & !o[i];
  }
  seitz_old.add_initial(I); // All gates are stable
  add_gate_old(seitz_old, req, !o[ack],         o[ack],          3, 3);
  add_gate_old(seitz_old, out, o[ack]!=o[req],  o[ack]==o[req],  1, 2);
  add_gate_old(seitz_old, ack, o[out] & o[req], o[out]& !o[req], 1, 3);
  seitz_old.add_cmd(varlist().add(z[0]),
		    varlist().add(zp[0]),
		    zp[0]<=z[0] & inv[0]);
  printf("Correct time\n");
  ddd R_old = seitz_old.poststar(I);
  printf("R_old => !hazard: %d\n", Consequence(R_old,!hazard));

  ddd B_old = seitz_old.prestar(hazard);
  printf("|/= B_old /\\ I: %d\n", Unsatisfiable(B_old & I));

  printf("R_old => R: %d\n", Consequence(R_old,R));
  printf("R => R_old: %d\n", Consequence(R,R_old));
  printf("B_old => B: %d\n", Consequence(B_old,B));
  printf("B => B_old: %d\n", Consequence(B,B_old));

  /*
  printf("Checking for hazards with ");
  printf("[%d,%d]\n",Tmin,Tmax);
  printf("\nProperty%s satisfied\n", seitz.check_property(!hazard) ? "":" NOT");
  */
}


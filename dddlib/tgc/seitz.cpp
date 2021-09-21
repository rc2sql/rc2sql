#include <stdio.h>
#include <stdlib.h>
#include <tgc.h>

#define A 9
#define C 10
#define B 11
#define REQIN 5
#define E 6
#define D 7
#define H 8
#define F 0
#define G 1
#define J 2
#define ACKOUT 3
#define I 4

const char* names[] = { "a", "c", "b", "reqin", "e", "d", "h", "f", "g", "j",
			"ackout", "i" };

boolean* o;
boolean* op;
boolean* u;
boolean* up;
real*    x;
real*    xp;
real z, zp, zpp;

ddd hazard;
ddd inv;

void add_gate(tgc& P, unsigned int i, const ddd& upguard, const ddd& dnguard,
	      unsigned int Tmin, unsigned int Tmax) {
  inv &= Forall(zpp, (zp<zpp & zpp<=z) >> !(!o[i] & !u[i] & upguard));
  inv &= Forall(zpp, (zp<zpp & zpp<=z) >> !(o[i] & !u[i] & dnguard));
  inv &= Forall(zpp, (zp<=zpp & zpp<=z) >> (u[i] >> (x[i]-zpp>=0 & x[i]-zpp<=Tmax)));

  P.add_cmd(varlist().add(u[i]).add(x[i]),
	    varlist().add(up[i]).add(xp[i]),
	    ((!o[i] & !u[i] & upguard) | (o[i] & !u[i] & dnguard)) & 
	    up[i] & xp[i]-z==0);

  P.add_cmd(varlist().add(u[i]).add(o[i]),//.add(x[i]),
	    varlist().add(up[i]).add(op[i]),//.add(xp[i]),
	    u[i] & x[i]-z>=Tmin & !up[i] & (o[i]!=op[i]));

  hazard |= (u[i] & !o[i] & !upguard) | (u[i] & o[i] & !dnguard);
}


int main(int argc, char* argv[]) {
  int i;
  int N = sizeof(names)/sizeof(char*);
  ddd::Init(6*N+3, 1024);

  o  = (boolean*)   calloc(N, sizeof(boolean));
  op = (boolean*)   calloc(N, sizeof(boolean));
  u  = (boolean*)   calloc(N, sizeof(boolean));
  up = (boolean*)   calloc(N, sizeof(boolean));
  x  = (real*)      calloc(N, sizeof(real));
  xp = (real*)      calloc(N, sizeof(real));

  z = "z";
  zp = "zp";
  zpp = "zpp";
  for (i=0; i<N; i++) {
    o[i] = boolean("%s", names[i]);
    op[i] = boolean("%s'", names[i]);
    u[i] = boolean("%s_u", names[i]);
    up[i] = boolean("%s_u'", names[i]);
    x[i] = real("%s_t", names[i]);
    xp[i] = real("%s_t'", names[i]);
  }

  ddd::ClearOrder();

  for (i=0; i<N; i++) {
    //    ddd::AppendToOrder(o[i]);      ddd::AppendToOrder(op[i]);
    //ddd::AppendToOrder(u[i]);      ddd::AppendToOrder(up[i]);
  }
  for (i=0; i<N; i++) {
    ddd::AppendToOrder(o[i]);      ddd::AppendToOrder(op[i]);
    ddd::AppendToOrder(u[i]);      ddd::AppendToOrder(up[i]);
    ddd::AppendToOrder(x[i], z);   ddd::AppendToOrder(xp[i], z);
    ddd::AppendToOrder(x[i], zp);  ddd::AppendToOrder(xp[i], zp);
    ddd::AppendToOrder(x[i], zpp); ddd::AppendToOrder(xp[i], zpp);
    for (int j=0; j<N; j++)
      if (i!=j) {
	ddd::AppendToOrder(x[i], x[j]);
	ddd::AppendToOrder(xp[i], x[j]);
	ddd::AppendToOrder(x[i], xp[j]);
	ddd::AppendToOrder(xp[i], xp[j]);
      }
  }
  ddd::AppendToOrder(z,zp);
  ddd::AppendToOrder(z,zpp);
  ddd::AppendToOrder(zp,zpp);
  ddd::OrderVariables(APPEND_INC_LEX);

  int Tmin=2;
  int Tmax=3;
  hazard = False;
  inv = True;

  if (argc>=3) {
    Tmin = atoi(argv[1]);
    Tmax = atoi(argv[2]);
  }

  tgc seitz;

  for (i=0; i<N; i++) {
    seitz.add_initial(!u[i]); // All gates are stable
  }

  seitz.add_initial(!o[REQIN] & o[A] & !o[B] & !o[C] & o[D] &  
  		    !o[E] & !o[F] & !o[G] & !o[H] & o[I] & o[J] & !o[ACKOUT]);
  add_gate(seitz, I,      !o[ACKOUT],      o[ACKOUT],        3*Tmin, 3*Tmax);
  add_gate(seitz, ACKOUT, o[G],            !o[G],            2*Tmin, 2*Tmin);
  add_gate(seitz, J,      o[I],            !o[I],            2*Tmin, 2*Tmax);
  add_gate(seitz, G,      o[F] & o[J],     o[F] & !o[J],     Tmin,Tmax);
  add_gate(seitz, F,      !o[D],           o[D],             Tmin,Tmax);
  add_gate(seitz, H,      o[I]==o[ACKOUT], o[I]!=o[ACKOUT],  Tmin,Tmax);
  add_gate(seitz, D,      !o[B] & !o[E],   o[B] | o[E],      Tmin,Tmax);
  add_gate(seitz, E,      !o[H] & !o[D],   o[H] | o[D],      Tmin,Tmax);
  add_gate(seitz, REQIN,  !o[C],           o[C],             3*Tmin, 3*Tmin);
  add_gate(seitz, B,      !o[A] & !o[E],   o[A] | o[E],      Tmin,Tmax);
  add_gate(seitz, C,      o[B] & o[REQIN], o[B] & !o[REQIN], Tmin,Tmax);
  add_gate(seitz, A,      o[C]==o[REQIN],  o[C]!=o[REQIN],   Tmin,Tmax);

  seitz.add_cmd(varlist().add(z),
		varlist().add(zp),
		zp<z & inv);

  printf("Checking for hazards with ");
  printf("[%d,%d]\n",Tmin,Tmax);
  printf("\nProperty%s satisfied\n", seitz.check_property(!hazard) ? "":" NOT");
}

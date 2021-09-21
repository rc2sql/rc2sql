#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <tgc.h>

static int N;

real *zpp, *zp, *z, *x, *xp;
boolean *b, *bp;
ddd *inv;
ddd *ini;

int main(int argc, char* argv[]) {
  N = argc>1 ? atoi(argv[1]) : 1;
  int i;

  ddd::Init(6*N, 64);
  b = (boolean*) calloc(N, sizeof(boolean));
  bp = (boolean*) calloc(N, sizeof(boolean));
  x = (real*) calloc(N, sizeof(real));
  xp = (real*) calloc(N, sizeof(real));
  z = (real*) calloc(N, sizeof(real));
  zp = (real*) calloc(N, sizeof(real));
  zpp = (real*) calloc(N, sizeof(real));
  inv = (ddd*) calloc(N, sizeof(ddd));
  ini = (ddd*) calloc(N, sizeof(ddd));

  for (i=0; i<N; i++) { 
    b[i] = boolean("b_%d",i);
    bp[i] = boolean("b'_%d",i);
    x[i] = real("x_%d",i);
    xp[i] = real("x'_%d",i);
    z[i] = real("z_%d",i);
    zp[i] = real("z'_%d",i);
    zpp[i] = real("z''_%d",i);
  }
  ddd::ClearOrder();

  for (i=0; i<N; i++) { 
    ddd::AppendToOrder(b[i]);
    ddd::AppendToOrder(bp[i]);
    ddd::AppendToOrder(x[i],z[i]);
    ddd::AppendToOrder(xp[i],z[i]);
    ddd::AppendToOrder(x[i],zp[i]);
    ddd::AppendToOrder(xp[i],zp[i]);
    ddd::AppendToOrder(x[i],zpp[i]);
    ddd::AppendToOrder(xp[i],zpp[i]);
  }
  ddd::OrderVariables(APPEND_INC_LEX);

  for (i=0; i<N; i++) {
    ini[i] = b[i] & (x[i]-z[i]==0);
    inv[i] = Forall(zpp[i], 
		    (zp[i]<=zpp[i] & zpp[i]<=z[i]) >> 
		    ((b[i] >> (x[i]-zpp[i]>=0 & x[i]-zpp[i]<4))));
  }

  /*
  tgc alpha1;
  ddd all_inv = True;
  for (i=0; i<N; i++) {
    alpha1.add_initial(Replace(ini[i],z[0],z[i]));
    all_inv &= Replace(Replace(Replace(inv[i],z[0],z[i]),zp[0],zp[i]),zpp[0],zpp[i]);
    alpha1.add_cmd(varlist().add(x[i]).add(b[i]),
		   varlist().add(xp[i]).add(bp[i]),
		   b[i] & bp[i] & x[i]-z[0]>=2 & xp[i]-z[0]==0);

  }
  alpha1.add_cmd(varlist().add(z[0]),
		 varlist().add(zp[0]),
		 zp[0]<=z[0] & all_inv);
  printf("Computing reachable state space for alpha1\n");
  ddd reach1 = alpha1.reachable_state_space();
  */

  tgc alpha2;
  for (i=0; i<N; i++) {
    alpha2.add_initial(ini[i]);
    alpha2.add_cmd(varlist().add(x[i]).add(b[i]),
		   varlist().add(xp[i]).add(bp[i]),
		   b[i] & bp[i] & x[i]-z[i]>=2 & xp[i]-z[i]==0);
  }    
  varlist v,vp;
  ddd expr = True;
  for (i=0; i<N; i++) {
    v.add(z[i]);
    vp.add(zp[i]);
    expr &= zp[i]<=z[i] & inv[i];
  }
  alpha2.add_cmd(v,vp,expr);

  printf("Computing reachable state space for alpha2\n");
  ddd reach2 = alpha2.reachable_state_space();
  for (i=1; i<N; i++)
    reach2 = Replace(reach2,z[0],z[i]);

  printf("\n|reach2| = %d\n", NodeCount(reach2));
  //  printf("\nreach1 = reach2: %d\n", Equivalent(reach1,reach2));
  //ViewGraph(reach2);
}



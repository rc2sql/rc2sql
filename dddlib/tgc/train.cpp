#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <tgc.h>

enumerator a, ap, b, bp;
real t, z, zp, zpp, x, xp, y, yp;

#define VG(f) SetGraphName(#f); ViewGraph(f)

void init() {
  ddd::Init(16, 8);
  a   = enumerator(3, "a");
  ap  = enumerator(3, "ap");
  b   = enumerator(4, "b");
  bp  = enumerator(4, "bp");
  t   = real("t");
  z   = real("z");
  zp  = real("zp");
  zpp = real("zpp");
  x   = real("x");
  xp  = real("xp");
  y   = real("y");
  yp  = real("yp");

  ddd::ClearOrder();

  ddd::AppendToOrder(a);
  ddd::AppendToOrder(ap);
  ddd::AppendToOrder(b);
  ddd::AppendToOrder(bp);

  ddd::AppendToOrder(z,zp);
  ddd::AppendToOrder(z,zpp);
  ddd::AppendToOrder(zp,zpp);
  ddd::AppendToOrder(t,z);
  ddd::AppendToOrder(t,zp);
  ddd::AppendToOrder(t,zpp);

  ddd::AppendToOrder(x, z);
  ddd::AppendToOrder(xp, z);
  ddd::AppendToOrder(x, zp);
  ddd::AppendToOrder(xp, zp);
  ddd::AppendToOrder(x, zpp);
  ddd::AppendToOrder(xp, zpp);

  ddd::AppendToOrder(y, z);
  ddd::AppendToOrder(yp, z);
  ddd::AppendToOrder(y, zp);
  ddd::AppendToOrder(yp, zp);
  ddd::AppendToOrder(y, zpp);
  ddd::AppendToOrder(yp, zpp);

  ddd::OrderVariables(APPEND_INC_LEX);
}

int main(int argc, char* argv[]) {
  init();

  tgc train;

  ddd I = a==0 & b==0 & x-z==0 & y-z==0;
  train.add_initial(I);


  ddd inv = (((a==1 | a==2) >> (x-z<=5)) 
	     & ((b==1) >> (y-z<2))
	     & ((b==3) >> (y-z<=1)));


  // a=0 /\ b=0 --> a=1,b=1,x=0,y=0
  train.add_cmd(varlist().add(a).add(b).add(x).add(y),
		varlist().add(ap).add(bp).add(xp).add(yp),
		a==0 & b==0 & ap==1 & bp==1 & xp-z==0 & yp-z==0);
  // a=1 /\ 2<x<=5 --> a=2
  train.add_cmd(varlist().add(a),
		varlist().add(ap),
		a==1 & 2<x-z & x-z<=5 & ap==2);
  // b=1 /\ 1<=y<2 --> b=2
  train.add_cmd(varlist().add(b),
		varlist().add(bp),
		b==1 & 1<=y-z & y-z<2 & bp==2);
  // a=2 /\ b=2 /\ x<=5  -->  a=0,b=3,y=0
  train.add_cmd(varlist().add(a).add(b).add(y),
		varlist().add(ap).add(bp).add(yp),
		a==2 & b==2 & x-z<=5 & ap==0 & bp==3 & yp-z==0);
  // b=3 /\ y<=1  -->  b=0
  train.add_cmd(varlist().add(b),
		varlist().add(bp),
		b==3 & y-z<=1 & bp==0);

  train.add_cmd(varlist().add(z),
		varlist().add(zp),
		zp<=z & Forall(zpp, (zp<=zpp & zpp<=z) >> Replace(inv,zpp,z)));

  ddd F = (t-z==0) >> (b==0);
  ddd X = True;
  ddd XP;
  do {
    XP = X;
    X = !train.pre(!X) & F;
    printf("[%d] ",NodeCount(X));
  } while (!Equivalent(X,XP));

  F = (b==2) >> Exists(t, (t-z==-1) >> X);

  X = False;
  do {
    XP = X;
    X = train.pre(X) | F;
    printf("[%d] ",NodeCount(X));
  } while (!Equivalent(X,XP));

  printf("Satisfied: %d\n", Tautology(I >> X));

}



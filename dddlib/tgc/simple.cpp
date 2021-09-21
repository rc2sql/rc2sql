#include <stdio.h>
#include <stdlib.h>
#include <tgc.h>


#define VG(f) SetGraphName(#f); ViewGraph(f)

#define ri (s1==0)
#define rh (s1==1)
#define ru (s1==2)
#define rw (s1==3)
#define wi (s2==0)
#define wh (s2==1)
#define wu (s2==2)
#define ww (s2==3)
#define pi (s3==0)
#define pu (s3==1)
#define pw (s3==2)
#define qi (s4==0)
#define qh (s4==1)
#define qu (s4==2)
#define qw (s4==3)

#define rip (s1p==0)
#define rhp (s1p==1)
#define rup (s1p==2)
#define rwp (s1p==3)
#define wip (s2p==0)
#define whp (s2p==1)
#define wup (s2p==2)
#define wwp (s2p==3)
#define pip (s3p==0)
#define pup (s3p==1)
#define pwp (s3p==2)
#define qip (s4p==0)
#define qhp (s4p==1)
#define qup (s4p==2)
#define qwp (s4p==3)



int main(int argc, char* argv[]) {
  ddd::Init(16,5);

  enumerator s1(4, "s1");
  enumerator s1p(4, "s1p");
  enumerator s2(4, "s2");
  enumerator s2p(4, "s2p");
  enumerator s3(3, "s3");
  enumerator s3p(3, "s3p");
  enumerator s4(4, "s4");
  enumerator s4p(4, "s4p");
  boolean d = "done";
  boolean dp = "donep";
  real total = "total";
  real totalp = "totalp";
  real r = "r";
  real w = "w";
  real p = "p";
  real q = "q";
  real rp = "rp";
  real wp = "wp";
  real pp = "pp";
  real qp = "qp";

  real z = "z";
  real zp = "zp";
  real zpp = "zpp";

  tgc A;

  ddd inv = True;
  ddd urg = ri & wi & pi & qi;

  A.add_initial(ri & !d & total-z==0);

  A.add_cmd(varlist().add(s1).add(r),
	    varlist().add(s1p).add(rp),
	    ri & (rhp | rup | rwp) & rp-z==0);
  inv &= (rh >> (r-z >=0 & r-z<=20));
  inv &= (ru >> (r-z >=0 & r-z<=25));
  inv &= (rw >> (r-z >=0 & r-z<=30));
  A.add_cmd(varlist().add(s1).add(s2),
	    varlist().add(s1p).add(s2p),
	    ((rh & r-z>=15) | (ru & r-z>=20) | (rw & r-z>=25)) & wip);

  A.add_cmd(varlist().add(s2).add(w),
	    varlist().add(s2p).add(wp),
	    wi & (whp | wup | wwp) & wp-z==0);
  inv &= (wh >> ((w-r>=20 & w-z >=0 & w-z<=15) | (w-r<20 & w-z >=0 & w-z<=20)));
  inv &= (wu >> ((w-r>=20 & w-z >=0 & w-z<=20) | (w-r<20 & w-z >=0 & w-z<=25)));
  inv &= (ww >> ((w-r>=20 & w-z >=0 & w-z<=25) | (w-r<20 & w-z >=0 & w-z<=30)));
  A.add_cmd(varlist().add(s2).add(s3),
	    varlist().add(s2p).add(s3p),
	    ((wh & ((w-r>=20 & w-z>=10)|(w-r<20 & w-z>=15))) |
	     (wu & ((w-r>=20 & w-z>=15)|(w-r<20 & w-z>=20))) |
	     (ww & ((w-r>=20 & w-z>=20)|(w-r<20 & w-z>=25)))) & pip);

  A.add_cmd(varlist().add(s3).add(p),
	    varlist().add(s3p).add(pp),
	    pi & (pup | pwp) & pp-z==0);
  inv &= (pu >> (p-z >=0 & p-z<=6));
  inv &= (pw >> (p-z >=0 & p-z<=4));
  A.add_cmd(varlist().add(s3).add(s4),
	    varlist().add(s3p).add(s4p),
	    (pu|pw) & qip);

  A.add_cmd(varlist().add(s4).add(q),
	    varlist().add(s4p).add(qp),
	    qi & (qhp | qup | qwp) & qp-z==0);
  inv &= ((qh|qu|qw) >> (q-z >=0 & q-z<=5));

  A.add_cmd(varlist().add(s4).add(d),
	    varlist().add(s4p).add(dp),
	    (qh|qu|qw) & ((q-p<4) >> (q-z>=3)) & dp);

  A.add_cmd(varlist().add(z),
	    varlist().add(zp),
	    zp<=z & 
	    Forall(zpp, ((zp<=zpp & zpp<=z) >> Replace(inv, zpp, z))) &
	    Forall(zpp, ((zp<zpp & zpp<=z) >> !Replace(urg, zpp, z))) );

  ddd R = A.reachable_state_space();

  printf("Nodecount(R) = %d\n", NodeCount(R & d));

  ViewGraph(R & d & total-z==33);

  printf("sat: %d\n", Satisfiable(R & d & total-z==33));

}

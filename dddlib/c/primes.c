#include "primes.h"
#include "memory.h"

/* The prime table */
static unsigned int* m_primes;

/* the number of primes between 0 and 65536 - (as anyone knows :-) ) */
static const unsigned int m_numPrimes = 6542;

void initPrimes(void) {
  /* Candidate for a prime */
  unsigned int c;

  /* Index to next prime */
  unsigned int indexC;

  m_primes = allocMem(m_primes, m_numPrimes * sizeof(unsigned int));

  /* The following while loop initializes m_primes using the
     Erastothene's Sieve algorithm.  For the loop it invariantly
     holds, that m_primes[0..indexC-1] has been initialized */
  c = 2;
  indexC = 0;
  while (indexC<m_numPrimes) { 
    /* Index to current divisor */
    unsigned int indexD;
    bool res;

    res = true;
    for (indexD = 0; indexD<indexC; indexD++) {
      /* Current divisor */
      /* Note that m_primes[indexD] has been initialized in a previous loop */
      unsigned int d = m_primes[indexD];

      /* no prime between 0 and ceil(sqrt(c)) divided c */
      if (d*d>c) 
	break;

      /* d divides c ==> c is not a prime */
      if (c%d==0) {
	res = false;
	break;
      }
    }

    if (res) {
      m_primes[indexC++] = c;
    }
    c++;
  }
}

void donePrimes(void) {
  freeMem(m_primes);
}

bool isPrime(unsigned int c) {
  unsigned int d;      /* Current divisor */
  unsigned int indexD; /* Index to current divisor */
  bool res = false;    /* Assume that the candidate is not a prime */

  if (c>1) {
    /* c>1
     ==> 2<=c<=2^32-1
     ==> (c is prime ==> no divisor d, 2<=d<=ceil(sqrt(2^32)), exists)
     ==> (c is prime ==> no divisor d, 2<=d<=65536, exists)
     ==> (c is prime ==> no prime-divisor d, 2<=d<=65536, exists)
     ==> (c is comp. <== a prime-divisor d, 2<=d<=65536, exists)
     ==> (c is comp. <== a divisor in m_primes exists)
     ==> (c is comp. <== a divisor d, in m_primes exists and d<=ceil(sqrt(c)))
    */

    for (indexD = 0; indexD<m_numPrimes; indexD++) {
      d = m_primes[indexD];

       /* no prime between 0 and ceil(sqrt(c)) divided c */
      if (d*d>c) {
	res = true;
	break;
      }

      /* d divides c ==> c is not a prime */
      if (c%d==0) {
	break;
      }
    }
    assert(indexD<m_numPrimes);
  }
  return res;
}

unsigned int nextPrime(unsigned int c) {
  /* Try all values from c to UINT_MAX (both inclusive),
     that is increment c until it gets zero. */
  while (c) {
    if (isPrime(c))
      break;
    c++;
  }
  return c;
}

unsigned int prevPrime(unsigned int c) {
  /* Try all values from c to 1 (both inclusive),
     that is decrement c until it gets zero. */
  while (c) {
    if (isPrime(c))
      break;
    c--;
  }
  return c;
}


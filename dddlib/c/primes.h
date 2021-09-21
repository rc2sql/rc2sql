#ifndef PRIMES_H
#define PRIMES_H

#include "types.h"

/* This module defines three functions for generating prime numbers
   (numbers that can only be divided by itself and 1) and for testing
   numbers for primality.  All functions use a variant of Erastothenes
   sieve where the prime numbers from 0 to 65536 are tabulated.  */

void initPrimes(void);
void donePrimes(void);

/* Return true if c is a prime */
bool isPrime(unsigned int c);

/* Return the largest prime that is smaller or equal than c (or 0 if
   there are no primes larger than c). */
unsigned int nextPrime(unsigned int c);

/* Return the smallest prime that is larger og equal than c (or 0 if
   there are no primes smaller than c). */
unsigned int prevPrime(unsigned int c);

#endif

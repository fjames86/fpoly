
#ifndef UTILS_H
#define UTILS_H

#include <stdlib.h>
#include <gmp.h>

#define MAX_FACTORIAL 500

mpz_t *factorial_table;
mpz_t tmp_1, tmp_2, tmp_3;

void factorial(mpz_t x, unsigned long int n);
void ncr(mpz_t x, unsigned long int n, unsigned long int k);


#endif

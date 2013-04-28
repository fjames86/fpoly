
#ifndef FPOLY_H
#define FPOLY_H

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <gmp.h>

#include "symbol.h"
#include "utils.h"

#define FPOLY_SYMBOLS 10

typedef struct {
	mpz_t *coeffs;
	int size;
	int degree;
	symbol *vars;
	int nvars;
} fpoly;

void fpoly_open();
void fpolt_close();

fpoly *make_fpoly(symbol *vars, int nvars, int degree);
void free_fpoly(fpoly *p);

int base_offset (int nvars, int degree);
int number_terms (int nvars, int degree);
int power_offset (int nvars, int *powers);
int offset (int nvars, int *powers);

mpz_t *fpoly_coeff (fpoly *p, int *powers);
void set_fpoly_coeff (fpoly *p, int *powers, mpz_t val);
void set_fpoly_coeff_si (fpoly *p, int *powers, long int val);
void fpoly_coeff_add(fpoly *p, int *powers, mpz_t val);
void fpoly_coeff_add_si (fpoly *p, int *powers, long int val);

void print_fpoly (fpoly *p);

#endif /* fpoly.h */

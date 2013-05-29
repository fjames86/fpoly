
#ifndef FPOLY_H
#define FPOLY_H

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <gmp.h>

#include "symbol.h"
#include "utils.h"

/* starting size of symbol stack */
#define FPOLY_SYMBOLS 5
#define FPOLY_DEGREE 20

typedef struct {
	mpz_t *coeffs;
	int size;
	int degree;
	symbol *vars;
	int nvars;

	/* power lists */
	int **powers;
} fpoly;

/*int *powers_table[FPOLY_SYMBOLS][FPOLY_DEGREE];*/

/* call open/close before/after using any other function */
void fpoly_open ();
void fpoly_close ();

/* make a poly object and free it */
fpoly *make_fpoly (symbol *vars, int nvars, int degree);
void free_fpoly (fpoly *p);

/* offset related functions */
int base_offset (int nvars, int degree);
int number_terms (int nvars, int degree);
int power_offset (int nvars, int *powers);
int offset (int nvars, int *powers);

/* various getters/setters */
mpz_t *fpoly_coeff (fpoly *p, int *powers);
void set_fpoly_coeff (fpoly *p, int *powers, mpz_t val);
void set_fpoly_coeff_si (fpoly *p, int *powers, long int val);
void mul_fpoly_coeff_si (mpz_t *p, long int val);

void fpoly_coeff_add (fpoly *p, int *powers, mpz_t val);
void fpoly_coeff_add_si (fpoly *p, int *powers, long int val);

/* printing routines */
void print_fpoly (fpoly *p);

#endif /* fpoly.h */

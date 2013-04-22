
#ifndef FPOLY_H
#define FPOLY_H

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <gmp.h>

#include "symbol.h"
#include "utils.h"

#define FPOLY_MAX_POOL 10

#define MAXVARS   5
#define MAXDEGREE 20

#define MAXCOEFFS 53130 /* lisp: (base-offset 5 20) = 53130 */

struct fpoly_t {
	/* list of variables */
	symbol *vars;
	int nvars;

	/* array of coefficients */
	int degree;
	int size; 
	mpz_t *coeffs;
};

/* pool of pre-allocated fpoly_t structs */
struct fpoly_t *fpoly_pool;

/* allocate the pool and initialise them */
void fpoly_open();
void fpoly_close();
void fpoly_init(struct fpoly_t *p, symbol *vars, int nvars, int degree);

/* get a poly struct from the pool and initialise it */
struct fpoly_t *make_fpoly(symbol *vars, int nvars, int degree);

#endif /* fpoly.h */


#ifndef FPOLY_H
#define FPOLY_H

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "symbol.h"
#include "utils.h"

/* list of variables */
struct var_t {
	symbol *var;
	struct var_t *next;
};


struct fpoly_t {
	/* list of variables */
	struct var_t *vars;
	int nvars;

	/* array of coefficients */
	int degree;
	int size; 
	int *coeffs;
};

struct var_t *make_var (symbol *x);
void free_vars (struct var_t *vars);

struct fpoly_t *make_fpoly (struct var_t *vars, int degree);
void free_fpoly (struct fpoly_t *p);


#endif /* fpoly.h */

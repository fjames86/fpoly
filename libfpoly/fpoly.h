
#ifndef FPOLY_H
#define FPOLY_H

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "symbol.h"

/* list of variables */
struct var_t {
	symbol *var;
	struct var_t *next;
};


struct fpoly_t {
	/* list of variables */
	var_t *vars;
	int nvars;

	/* array of coefficients */
	int degree;
	int size; 
	int *coeffs;
};



#endif /* fpoly.h */

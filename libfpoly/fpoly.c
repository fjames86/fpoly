
#include "fpoly.h"

/* ---------------------- init ----------------- */

void fpoly_open () {
	int i;
	
	symbol_init (FPOLY_SYMBOLS);
	mpz_init(tmp_1);
	mpz_init(tmp_2);
	mpz_init(tmp_3);

	factorial_table = (mpz_t *)malloc(sizeof(mpz_t)*MAX_FACTORIAL);	
	for(i=0; i < MAX_FACTORIAL; ++i) {
		mpz_init(factorial_table[i]);
		mpz_fac_ui(factorial_table[i], i);
	}	
}

void fpoly_close () {
	int i;
	/* free all tables etc */

	mpz_clear(tmp_1);
	mpz_clear(tmp_2);
	mpz_clear(tmp_3);

	for(i=0; i < MAX_FACTORIAL; ++i) {
		mpz_clear(factorial_table[i]);
	}
	free(factorial_table);
}

/* ---------------- make the object ---------------- */

fpoly *make_fpoly(symbol *vars, int nvars, int degree) {
	fpoly *p;
	int i;
	
	p = (fpoly *)malloc(sizeof(fpoly));

	/* set the variabels */
	p->nvars = nvars;
	p->vars = (symbol *)malloc(sizeof(symbol)*nvars);	
	for(i=0; i < nvars; ++i) {
		p->vars[i] = vars[i];
	}
	
	/* set the coefs */
	p->degree = degree;
	p->size = base_offset(nvars, degree);
	p->coeffs = (mpz_t *)malloc(sizeof(mpz_t)*p->size);

	for (i=0; i < p->size; ++i) {
		mpz_init (p->coeffs[i]);
	}

	return p;
}

void free_fpoly (fpoly *p) {
	int i;
	
	free (p->vars);

	for(i=0; i < p->size; i++) {
		mpz_clear(p->coeffs[i]);
	}
	
	free (p->coeffs);	
	free(p);
}

/* -------------------------- offset calculations  ---------------------------- */

int base_offset (int nvars, int degree) {
	if (degree < 0) {
		return 0;
	} else {
		return ncr(nvars + degree, nvars);
	}
}

int number_terms (int nvars, int degree) {
	if (degree == 0) {
		return 1;
	} else {
		return base_offset(nvars, degree) - base_offset(nvars, degree-1);
	}
}

int power_offset (int nvars, int *powers) {
	int total_degree, partial_degree, power;
	int i;

	total_degree = powers[0];
	partial_degree = 0;
	for(i=1; i < nvars; ++i) {
		total_degree += powers[i];
		partial_degree += powers[i];
	}

	power = powers[0];
	
	if (nvars == 1 || total_degree == 0) {
		return 0;
	} else {
		return number_terms (nvars, partial_degree - 1) + power_offset (nvars - 1, &powers[1]);
	}
}

/*
	} else if (power == 0) {
		return number_terms (nvars, degree - 1) + power_offset (nvars - 1, &powers[1]);
	} else {
		powers[0] -= 1;
		return power_offset(nvars, powers);
	}
}
*/

int offset(int nvars, int *powers) {
	int degree, i;
	degree = 0;
	for(i=0; i < nvars; ++i) {
		degree += powers[i];
	}
	
	return base_offset(nvars, degree - 1) + power_offset(nvars, powers);
}

		

/* ----------------- getters and setters --------------------------------------- */

mpz_t *fpoly_coeff (fpoly *p, int *powers) {
	return &p->coeffs[offset(p->nvars, powers)];
}

void set_fpoly_coeff(fpoly *p, int *powers, mpz_t val) {
	mpz_set (p->coeffs[offset(p->nvars, powers)], val);	
}

void set_fpoly_coeff_si (fpoly *p, int *powers, long int val) {
	mpz_set_si (p->coeffs[offset(p->nvars, powers)], val);
}

void fpoly_coeff_add (fpoly *p, int *powers, mpz_t val) {
	mpz_add(p->coeffs[offset(p->nvars, powers)], p->coeffs[offset(p->nvars, powers)], val);
}

void fpoly_coeff_add_si (fpoly *p, int *powers, long int val) {
	mpz_add_si(p->coeffs[offset(p->nvars, powers)], p->coeffs[offset(p->nvars, powers)], val);
}

/* ------------------------- printer ------------------------- */

void print_fpoly (fpoly *p) {
	int i;

	printf("#<FPOLY :VARS ");
	printf("(");
	for(i=0; i < p->nvars; ++i) {
		if (i > 0) printf (" ");
		printf("%s", p->vars[i]);
	}
	printf (")");
	
	printf (" :DEGREE %d", p->degree);

	printf (" :COEFFS #(");
	for(i=0; i < p->size; ++i) {
		if (i > 0) printf (" ");
		mpz_out_str(stdout, 10, p->coeffs[i]);
	}
	
	printf(")>");
}

/* ---------------------------------------- */


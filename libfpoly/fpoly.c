
#include "fpoly.h"

struct var_t *make_var (symbol *x) {
	struct var_t *var;

	var = (struct var_t *) malloc(sizeof(struct var_t));
	var->var = x;
	var->next = NULL;
}

void free_vars (struct var_t *vars) {
    if (vars->next) {
		free_vars(var->next);
	}
	free(vars);
}

struct fpoly_t *make_fpoly (struct var_t *vars, int degree) {
	struct fpoly_t *p;
	struct var_t *v;
	int i;
	
	p = (struct fpoly_t *) malloc (sizeof (struct fpoly_t));
	p->vars = vars;
	p->nvars = 0;

	v = p->vars;
	while(v) {
		p->nvars++;
	}

	p->degree = degree;
	p->size = ncr(degree + p->nvars, degree);
	p->coeffs = (int *) malloc (sizeof(int)*p->size);

	for(i=0; i < p->size; i++) {
		p->coeffs[i] = 0;
	}

	return p;
}

void free_fpoly (struct fpoly_t *p) {
	free_vars (p->vars);
	free (p->coeffs);
	free(p);
}

		


#include "fpoly.h"

void fpoly_open() {
	int i;
	struct fpoly_t *p;
	
	fpoly_pool = make_stack(FPOLY_MAX_POOL);
	for(i=0; i < FPOLY_MAX_POOL; i++) {
		p = new_fpoly();
		stack_push(&fpoly_pool, p);
	}	
}

void fpoly_close() {
	int i;

	for(i=0; i < fpoly_pool->i; i++) {
		delete_fpoly(stack_pop(fpoly_pool));
	}
	
	free_stack(fpoly_pool);
}

void fpoly_init(struct fpoly_t *p, symbol *vars, int nvars, int degree) {
	int i;

	p->nvars = nvars;
	p->vars = (symbol *)malloc(sizeof(symbol)*nvars);
	for(i=0; i < nvars; i++) {
		p->vars[i] = vars[i];
	}
   
	p->degree = degree;
	p->size = base_offset(nvars, degree);
	p->coeffs = (mpz_t *)malloc(sizeof(mpz_t)*p->size);
	for(i=0; i < p->size; i++) {
		mpz_init(&p->coeffs[i]);
	}
	
}

struct fpoly_t *make_fpoly() {

}

void free_fpoly (struct fpoly_t *p) {

}


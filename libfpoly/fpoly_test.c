
#include <stdio.h>
#include <stdlib.h>

#include "fpoly.h"

int main(int argc, char **argv) {
	fpoly *p;
	symbol vars[2];
	int powers[2];
	mpz_t val;
	
	/* initialise */
	fpoly_open();

	for (powers[0]=0; powers[0] < 3; powers[0]++) {
		for(powers[1]=0; powers[1] < 3; powers[1]++) {
			printf("(%d %d) = %d\n", powers[0], powers[1], offset(2, powers));
		}
	}
	
	/* set the variables */
	vars[0] = intern("X");
	vars[1] = intern("Y");

	/* make the poly */
	p = make_fpoly(vars, 2, 2);

	powers[0] = 1;
	powers[1] = 1;
	set_fpoly_coeff_si (p, powers, 3);

	powers[0] = 2;
	powers[1] = 0;
	set_fpoly_coeff_si (p, powers, -1);

	
	/* print it */
	print_fpoly(p);

	printf ("\n");
	
	/* clean up */
	free_fpoly(p);
	fpoly_close();
	
	exit(EXIT_SUCCESS);
}

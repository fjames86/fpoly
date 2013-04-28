
#include <stdio.h>
#include <stdlib.h>

#include "fpoly.h"

int main(int argc, char **argv) {
	fpoly *p;
	symbol vars[2];
	
	/* initialise */
	fpoly_open();

	/* set the variables */
	vars[0] = intern("X");
	vars[1] = intern("Y");

	/* make the poly */
	p = make_fpoly(vars, 2, 2);

	/* print it */
	print_fpoly(p);

	printf ("\n");
	
	/* clean up */
	free_fpoly(p);
	fpoly_close();
	
	exit(EXIT_SUCCESS);
}

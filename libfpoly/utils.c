
#include "utils.h"

void factorial (mpz_t x, unsigned long int n) {
	int i, ret;;
	if (n < MAX_FACTORIAL) {
		mpz_set(x, factorial_table[n]);
	} else {
		mpz_fac_ui(x, n);
	}
}

/* return factorial(n)/(factorial(k)*factorial(n - k)); */
void ncr (mpz_t x, unsigned long int n, unsigned long int k) {
	factorial (tmp_1, k);
	factorial (tmp_2, n-k);
	mpz_mul(tmp_3, tmp_1, tmp_2);
	
	factorial (tmp_1, n);
	
	mpz_cdiv_q(x, tmp_1, tmp_3);
}





	
			

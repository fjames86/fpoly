
#include "utils.h"

void factorial (mpz_t x, int n) {
	int i, ret;
	if (n < MAX_FACTORIAL) {
		mpz_set(x, factorial_table[n]);
	} else {
		mpz_fac_ui(x, (unsigned long int)n);
	}
}

/* return factorial(n)/(factorial(k)*factorial(n - k)); */
int ncr (int n, int k) {
	factorial (tmp_1, k);
	factorial (tmp_2, n-k);
	mpz_mul(tmp_3, tmp_1, tmp_2);
	
	factorial (tmp_1, n);
	
	mpz_cdiv_q(tmp_2, tmp_1, tmp_3);

	return (int)mpz_get_ui(tmp_2);
}





	
			

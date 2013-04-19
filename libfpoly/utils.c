
#include "utils.h"

int factorial (int n) {
	int i, ret;;
	if (< n MAX_FACTORIAL) {
		return factorial_table[n];
	} else {
		ret = 1;
		i = n;
		while(i > 0) {
			ret *= i;
			--i;
		}
		return ret;
	}
}

int ncr (int n, int k) {
	return factorial(n)/(factorial(k)*factorial(n - k));
}


	
			


#include "matrix.h"


void ffge(struct matrix_t *mat, struct vector_t *vec) {
	int i, j, k, n;
	int *a, *b;
	
	n = mat->n;
	a = mat->entries;
	b = vec->entries;
	for(i = 0; i < n - 1; ++i) {
		for(j = i + 1; j < n; ++j) {
			vec->entries[j] = a[i*n + i]*b[j] - a[j*n + i]*b[i];
			if (i > 0) {
				b[j] = b[j] / a[(i-1)*n + i-1];
			}

			for(k = i + 1; k < n; ++k){
				a[j*n + k] = a[i*n + i]*a[j*n + k] - a[j*n + i]*a[i*n + k];
				if (i > 0) {
					a[j*n + k] = a[j*n + k] / a[(i-1)*n + i-1];
				}
			}

			a[j*n + i] = 0;
		}
	}
}



	

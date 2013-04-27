
#include "matrix.h"

void ffge(int *a, int *b, int n) {
	int i, j, k;

	for(i = 0; i < n - 1; ++i) {
		for(j = i + 1; j < n; ++j) {
			b[j] = a[i*n + i]*b[j] - a[j*n + i]*b[i];
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

void ffge_list (int *mats, int *vecs, int num, int n) {
	int i;

	for(i=0; i < num; i++) {
		ffge(mats + num*n*n, vecs + num*n, n);
	}
}



	


#include "matrix.h"

int ffge(int *a, int *b, int n) {
	int i, j, k;

	for(i = 0; i < n - 1; ++i) {
		if (aref(a, i, i, n) == 0) {
			/* need to pivot */
			if (pivot(a, b, i, n)) {
				/* can't pivot! probably will divide by zero so return an error code */
				return 1;
			}
		}
		
		for(j = i + 1; j < n; ++j) {
			b[j] = aref(a, i, i, n)*b[j] - aref(a, j, i, n)*b[i];
			if (i > 0) {
				b[j] = b[j] / aref(a, i-1, i-1, n);
			}

			for(k = i + 1; k < n; ++k){
				aref(a, j, k, n) = aref(a, i, i, n)*aref(a, j, k, n) - aref(a, j, i, n)*aref(a, i, k, n);
				if (i > 0) {
					aref(a, j, k, n) = aref(a, j, k, n) / aref(a, i-1, i-1, n);
				}
			}

			aref(a, j, i, n) = 0;
		}
	}

	return 0;
}

int pivot(int *a, int *b, int i, int n) {
	int row, col, tmp;

	int err = 1;
	for(row=i; row < n; ++row) {
		if (aref(a, row, i, n) != 0) {
			/* found a row with non-zero pivot element */
			for(col=0; col < n; ++col) {
				tmp = aref(a, i, col, n);
				aref(a, i, col, n) = aref(a, row, col, n); 
				aref(a, row, col, n) = tmp;

				tmp = b[i];
				b[i] = b[row];
				b[row] = tmp;
			}
			
			err = 0;
			break;
		}
	}
	
	return err;
}
void ffge_list (int *mats, int *vecs, int num, int n) {
	int i, j, k;
	int err;
	int *a, *b;
	
	for(i=0; i < num; ++i) {
		err = ffge(mats + i*n*n, vecs + i*n, n);
		if (err == 1) {
			/* unsolveable matrix, set it all to zero */
			a = mats + i*n*n;
			b = vecs + i*n;
			for(j=0; j < n; ++j) {
				for(k=0; k < n; ++k) {
					aref(a, j, k, n) = 0;
				}
				b[j] = 0;
			}
		}
	}
}



	

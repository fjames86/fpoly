
#include "matrix.h"

/* ---------------------------------------------------- */

/* echelon */

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


/* ---------------------------------------------- */

/* lu decomposition */

int lu_decompose (int *u, int *l, int *p, int *dd, int *matrix, int n) {
	int oldpivot = 1;
	int nswaps = 0;
	int i, j, k, col;
	int kpivot;
	int notfound;
	int tmp;
	
	/* set up matrices */
	for(i=0; i < n; i++) {
		for(j=0; j < n; j++) {
			aref(u, i, j, n) = aref(matrix, i, j, n);

			if (i == j) {
				aref(p, i, j, n) = 1;
				aref(l, i, j, n) = 1;
			} else {
				aref(p, i, j, n) = 0;
				aref(l, i, j, n) = 0;
			}
		}
	}
			   

	
	for (k=0; k < n - 1; k++) {
		if (aref(u, k, k, n) == 0) {
			kpivot = k + 1;
			notfound = 1;
			while ((kpivot < n) && notfound) {
				if (aref(u, kpivot, k, n) != 0) {
					notfound = 0;
					kpivot++;
				}
			}
			if (kpivot == n) {
				/* error */
			} else {
				for (col = k; col < n; col++) {
					tmp = aref(u, kpivot, col, n);
					aref(u, kpivot, col, n) = aref(u, k, col, n);
					aref(u, k, col, n) = tmp;

					tmp = aref(p, kpivot, col, n);
					aref(p, kpivot, col, n) = aref(p, k, col, n);
					aref(p, k, col, n) = tmp;
				}
			}

			nswaps++;
		}

		aref(l, k, k, n) = aref(u, k, k, n);
		dd[k] = oldpivot*aref(u, k, k, n);

		for(i=k + 1; i < n; i++) {
			aref(l, i, k, n) = aref(u, i, k, n);

			for(j = k + 1; j < n; j++) {
				aref(u, i, j, n) = ((aref(u, k, k, n) * aref(u, i, j, n)) - (aref(u, k, j, n)*aref(u, i, k, n))) / oldpivot;
			}

			aref(u, i, k, n) = 0;
		}

		oldpivot = aref(u, k, k, n);
	}

	dd[n-1] = oldpivot;

	return nswaps;
}


int det(int *matrix, int n) {
	int *u, *l, *p, *dd;
	int nswaps;
	int dt, i;
	
	u = (int *)alloca(sizeof(int)*n*n);
	l = (int *)alloca(sizeof(int)*n*n);
	p = (int *)alloca(sizeof(int)*n*n);
	dd = (int *)alloca(sizeof(int)*n);

	nswaps = lu_decompose(u, l, p, dd, matrix, n);

	dt = (((nswaps % 2) == 0) ? 1 : -1);

	for(i=0; i < n; i++) {
		dt *= aref(u, i, i, n);
		dt *= aref(l, i, i, n);
		dt /= dd[i];
	}

	return dt;
}


		
	

	
	

	

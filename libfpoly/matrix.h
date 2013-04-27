
#ifndef MATRIX_H
#define MATRIX_H

#define aref(mat, row, col, n) (mat[(col)*(n) + (row)])

int ffge(int *mat, int *vec, int n); 
void ffge_list (int *mats, int *vecs, int num, int n);

int pivot(int *a, int *b, int i, int n);
	

#endif

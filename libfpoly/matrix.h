
#ifndef MATRIX_H
#define MATRIX_H

#include <stdlib.h>

#define aref(mat, row, col, n) (mat[(col)*(n) + (row)])

int ffge(int *mat, int *vec, int n); 
void ffge_list (int *mats, int *vecs, int num, int n);
int pivot(int *a, int *b, int i, int n);

int lu_decompose (int *u, int *l, int *p, int *dd, int *matrix, int n);

long int det(int *matrix, int n);


#endif

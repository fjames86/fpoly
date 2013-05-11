
#ifndef MATRIX_H
#define MATRIX_H

#include <alloca.h>

#define aref(mat, row, col, n) (mat[(row)*(n) + (col)])

int ffge(int *mat, int *vec, int n); 
void ffge_list (int *mats, int *vecs, int num, int n);
int pivot(int *a, int *b, int i, int n);

int lu_decompose (int *u, int *l, int *p, int *dd, int *matrix, int n);

int det(int *matrix, int n);


#endif

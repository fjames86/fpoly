
#ifndef MATRIX_H
#define MATRIX_H

struct matrix_t {
	int *entries;
	int n;
};

struct vector_t {
	int *entries;
	int n;
};
	
void ffge(struct matrix_t *mat, struct vector_t *vec);

	

#endif

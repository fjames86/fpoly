
/*
 * compiles on elephanttest using
 * nvcc --compiler-options '-fPIC' -o libfpoly.so --shared matrix.cu
 */
 
#include <cuda_runtime.h>

#define aref(mat, row, col, n) (mat[(col)*(n) + (row)])

/* do the echelon operation */
__device__ int ffge(int *mat, int *vec, int n);

/* launch the threads on the GPU */
__global__ int ffge_gpu(int *mats, int *vecs, int num, int n);

/* pivoting rows */
__device__ int pivot(int *a, int *b, int i, int n);

/* the entry point for calling */
void ffge_list (int *mats, int *vecs, int num, int n);


/* a = matrix, b = vector */
__device__ int ffge(int *a, int *b, int n) {
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

__device__ int pivot(int *a, int *b, int i, int n) {
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

__global__ void ffge_gpu(int *mats, int *vecs, int num, int n) {
	int i = blockDim.x * blockIdx.x + threadIdx.x;
	int mat_size = sizeof(int)*n*n;
	int vec_size = sizeof(int)*n;
	
	if (i < num) {
		ffge(mats + i*mat_size, vecs + i*vec_size, n);
	}
}

int ffge_list(int *h_mats, int *h_vecs, int num, int n) {
	int mat_size = sizeof(int)*n*n;
	int vec_size = sizeof(int)*n;
	 int *d_mats, *d_vecs;
	
	 /* cudaMalloc the arrays and copy over to the GPU */
	 cudaError_t err = cudaSuccess;
	 
	 err = cudaMalloc((void **)&d_mats, num*mat_size);
	 if (err != cudaSuccess) {
	 	return 1;
	 }
	 
	 err = cudaMalloc((void **)&d_vecs, num*vec_size);
	 if (err != cudaSuccess) {
	 	return 1;
	 }

	 /* now copy the data over */
	 err = cudaMemcpy(d_mats, h_mats, num*mat_size, cudaMemcpyHostToDevice);
	 if (err != cudaSuccess) {
	 	return 1;
	 }
	 
	 err = cudaMemcpy(d_vecs, h_vecs, num*vec_size, cudaMemcpyHostToDevice);
	 if (err != cudaSuccess) {
	 	return 1;
	 }

	 /* do the computation on the GPU */
	 ffge_gpu<<<1,num>>>(d_mats, d_vecs, num, n);

	 /* now copy the results back to the host memory */

	 err = cudaMemcpy(h_mats, d_mats, num*mat_size, cudaMemcpyDeviceToHost);
	 if (err != cudaSuccess) {
	 return 1;
	 }
	 
	 err = cudaMemcpy(h_vecs, d_vecs, num*vec_size, cudaMemcpyDeviceToHost);
	 if (err != cudaSuccess) {
	 	return 1;
	}

	 err = cudaFree(d_mats);
	 if (err != cudaSuccess) {
	 return 1;
	 }
	 
	 err = cudaFree(d_vecs);
	 if (err != cudaSuccess){
	 	return 1;
	}

	 return 0;
}

	

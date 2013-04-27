
/*
 * compiles on elephanttest using
 * nvcc --compiler-options '-fPIC' -o libfpoly.so --shared matrix.cu
 */
 
#include <cuda_runtime.h>

/* a = matrix, b = vector */
__device__ void ffge(int *a, int *b, int n) {
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

	

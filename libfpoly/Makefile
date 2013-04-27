
libfpoly.so: matrix.o
	gcc -shared -fPIC -o libfpoly.so matrix.o

matrix.o: matrix.c matrix.h
	gcc -c -fPIC matrix.c

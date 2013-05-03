CC=gcc
CFLAGS=-c -fPIC

libfpoly.so: fpoly.o matrix.o memory.o symbol.o utils.o
	${CC} -shared -fPIC -lgmp -o libfpoly.so fpoly.o matrix.o symbol.o memory.o utils.o

matrix.o: matrix.c matrix.h
	${CC} ${CFLAGS} matrix.c

fpoly.o: fpoly.c fpoly.h utils.h memory.h symbol.h 
	${CC} ${CFLAGS} fpoly.c

utils.o: utils.c utils.h
	${CC} ${CFLAGS} utils.c

symbol.o: symbol.c symbol.h memory.h
	${CC} ${CFLAGS} symbol.c

memory.o: memory.c memory.h
	${CC} ${CFLAGS} memory.c

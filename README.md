fpoly
=====

A package for manipulation of dense multivariate polynomials. Contains routines for:

* Making and manipulating polynomials
* Basic operations on polynomials (addition, subtraction, multiplication etc.)
* Making matrices of polynomials
* Assorted operations on polynomial matrices
* Lagrange Interpolation
* Foreign function interface to a C library providing extra features

In the future I intend to add functions to the C library for improved performance options.


To use the C library, first make it using the provided Makefile (note: libfpoly does
not currently use all *.c files in the libfpoly directory).
See examples/ffi-example.lisp for an example of it in use.


Frank James


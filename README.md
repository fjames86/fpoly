fpoly
=====

A package for manipulation of dense multivariate polynomials. Contains routines for:

* Making and manipulating polynomials
* Basic operations on polynomials (addition, subtraction, multiplication etc.)
* Making matrices of polynomials
* Assorted operations on polynomial matrices
* Lagrange Interpolation
* Foreign function interface to a C library providing extra features
* Parser and reader macro #{ } allowing entering polynomials in human-readable form

There is now a small C library of functions which mirror the Lisp routines. These
will allow inter-operation between the nice and easy to use Lisp front-end and a more
efficient C library for operations if speed is required.

To use the C library, first make it using the provided Makefile (note: libfpoly does
not currently use all *.c files in the libfpoly directory).
See examples/ffi-example.lisp for an example of it in use.

There is a reader macro defined, #{ }, allowing entering of human readable format
polynomials, e.g. #{1 + 2x^2} -> #<FPOLY :VARS (X) :DEGREE 2 :COEFFS #(1 0 2)>
Note that the parser can only cope with single-character variable names.
This is due to the ambiguity of terms like "xyz", is this equal to multplying three variables,
X, Y, Z or two variables XY, Z etc. If long names are required then they have to be set
by a substitute call, e.g. we want 1 + <foo>^2 so we do
(fpoly-substitute 'x 'foo #{1 + 2x^2}) -> #<FPOLY :VARS (FOO) :DEGREE 2 :COEFFS #(1 0 2)>



Frank James


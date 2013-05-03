FPOLY
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


Getting started
---------------

* Entering polynomials

Create polynomial objects with the (make-fpoly <vars> <degree>) function. This returns
a polynomials in the variables and degree specified, with all coeffieients set to zero.
You may then set coeffients using (setf (fpoly-coeff ...) Val) e.g.

(defvar p (make-fpoly '(x y) 2)) ; in X,Y of degree 2
(setf (fpoly-coeff poly 1 1) 2) ; set the coeff in front of the XY term to 2

There is a reader macro defined for convenience, #{ },
allowing entering of human readable format polynomials, e.g.
#{1 + 2x^2} -> 1 + 2X^2 ;; equivalent to (make-fpoly 'X 2 '(1 0 2))

Note that the parser can only cope with single-character variable names.
This is due to the ambiguity of terms like "xyz", is this equal to multplying three variables,
X, Y, Z or two variables XY, Z etc. If long names are required then they have to be set
by a substitute call, e.g. we want 1 + FOO^2 so we do
(princ (fpoly-substitute 'x 'foo #{1 + 2x^2})) 1 + 2*X^2

Note also that the parser is robust enough to cope with nested subexpressions, e.g.
#{x(1 + y)} -> X + XY


* Operations

There are functions for the standard arithmetic operations addition (fpoly-add) subtraction (fpoly-sub), multiplication (fpoly-mul) and also exponentiation (fpoly-expt).
Use these just like the standard functions + - * expt
e.g. (fpoly-add #{1 + x^2} #{1 - y^2}) -> 2 + X^2 - Y^2

There are also other functions for evaluation, substitution and simplification of polynomials

* Matrices

There are a set of functions for manipulating matrices of polynomials.

* Interpolation

The function lagrange-interpolate computes a polynomial which goes through a set of points.

* libfpoly interface

A set of CFFI bindings to the partner C library are also provided. The functions provided by
the C library are a subset of those available in Lisp; it should be used only where speed
is desired (and even then should be tested to ensure it really is faster).
Lisp is the lead platform.


Frank James


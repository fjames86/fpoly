FPOLY
=====

A package for manipulation of dense multivariate polynomials. Contains routines for:

* Making and manipulating polynomials
* Basic operations on polynomials (addition, subtraction, multiplication etc.)
* Assorted operations on matrices of polynomials
* Lagrange Interpolation
* Foreign function interface to a C library providing extra features
* Parser and reader macro #{ } allowing entering polynomials in human-readable form
* Solving systems of linear equations with polynomial coefficients

There is now a small C library of functions which mirror the Lisp routines. These
will allow inter-operation between the nice and easy to use Lisp front-end and a more
efficient C library for operations if speed is required.

To use the C library, first make it using the provided Makefile
See examples/ffi-example.lisp and libfpoly/fpoly_test.c for an example of it in use.


Getting started
---------------

* Entering polynomials

Create polynomial objects with the (make-fpoly <vars> <degree>) function. This returns
a polynomials in the variables and degree specified, with all coeffieients set to zero.
You may then set coeffients using (setf (fpoly-coeff ...) Val) e.g.
<br> (defvar p (make-fpoly '(x y) 2))  in X,Y of degree 2
<br> (setf (fpoly-coeff poly 1 1) 2)   set the coeff in front of the XY term to 2

There is a reader macro defined for convenience, \#{ },
allowing entering of human readable format polynomials, e.g.
<br> \#{1 + 2x^2} -> 1 + 2X^2   equivalent to (make-fpoly 'X 2 '(1 0 2))

Note that the parser can only cope with single-character variable names.
This is due to the ambiguity of terms like "xyz", is this equal to multplying three variables,
X, Y, Z or two variables XY, Z etc. If long names are required then they have to be set
by a substitute call, e.g. we want 1 + FOO^2 so we do
<br> (princ (fpoly-substitute 'x 'foo \#{1 + 2x^2})) 1 + 2*X^2

Note also that the parser is robust enough to cope with nested subexpressions, e.g.
<br> \#{x(1 + y)} -> X + XY


* Operations

There are functions for the standard arithmetic operations addition (fpoly-add) subtraction (fpoly-sub), multiplication (fpoly-mul) and also exponentiation (fpoly-expt).
Use these just like the standard functions + - * expt, e.g.
<br> (fpoly-add \#{1 + x^2} \#{1 - y^2}) -> 2 + X^2 - Y^2

There are also other functions for evaluation, substitution and simplification of polynomials

* Matrices

Matrices of polynomials are used to represent systems of linear equations with polynomial coefficients. These can be loaded from files using (load-matrix "filename").

For testing purposes you can define a random system using the macro

(def-test-system (matrix-name values-name) variables max-degree max-coeff n &key entry-density coeff-density vals)

This will define an n x n system of equations with density (probability of being non-zero) entry-density. The coefficients in the randomly generated polynomials have a density also.

E.g. define a nice and simple test system

(def-test-system (mymat myvals) '(x y) 3 10 3 :vals '(1 2 3))

This defines using defparameter a matrix called mymat and a set of values (solution to the system of equations) myvals, in this case '(1 2 3). If not specified then they will be random.


<br>
<br>

There are three approaches to solving.

1. Solving systems of equations can be done directly using direct-solve.

This will, using the standard operationss fpoly-add etc., transform the input matrix into upper triangular form and then solve for each of the system values, if possible.

So with the above system (which we know to have a solution of '(1 2 3)) we get

(direct-solve mymat) -> (#{1} #{2} #{3})


2. simple-solve takes a different approach. It first chooses some (random) evaluation points to generate a set of n matrices of numbers only. These are transformed to upper triangular form and then recombined back into polynomials using lagrange interpolation.

The advantage being that the matrix operations involve matrices of bignums only rather than polynomials.


3. solve-system takes it a step further and repeat the procedure of simple-solve but working modulo primes. This constrains the extent of the (bignum) coefficients so that the matrix operations can be guaranteed to in terms of fixnums only. This means that the matrix operations can be done on a GPU.

At present, solve-system does not work.

<br>
<br>

* Interpolation

The function lagrange-interpolate computes a polynomial which goes through a set of points.

This requires finding the determinant of a set of n x n matrices which can grow very large for even moderately sized input problems. Here, n = (base-offset nvars degree). Be warned, this requires finding determinants of large matrices and will therefore become very slow very quickly!

* libfpoly interface

A set of CFFI bindings to the partner C library are also provided. The functions provided by
the C library are a subset of those available in Lisp; it should be used only where speed
is desired (and even then should be tested to ensure it really is faster).
Lisp is the lead platform.


<br>
<br>
Frank James


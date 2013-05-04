
;;;; read in a matrix/vector system of polynomials describing a set of
;;;; simultaneous equations
;;;; 

(defpackage #:solve-linear-system
  (:use #:cl #:fpoly))

(in-package #:solve-linear-system)

(defvar mymat (load-matrix "../examples/test-mat.dat"))

(multiple-value-bind (mat-lists primes bindings) (generate-matrices mymat)
  (defvar mat-list mat-lists)
  (defvar primes primes)
  (defvar bindings bindings))

(defvar solved-mats (mapcan (lambda (row v)
							  (mapcar (lambda (m)
										(ffge m v))
									  row))
							mat-list
							vec-list))


									 


	  

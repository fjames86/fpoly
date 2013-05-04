
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

(defvar solved-mats (mapcar (lambda (mats)
							  (mapcar (lambda (mat)
										(echelon mat))
									  mats))
							mat-list))

(defvar interpolated-matrices (mapcar (lambda (mats binding)
										(lagrange-interpolate-matrix mats binding 2))
									  solved-mats bindings))




									 


	  

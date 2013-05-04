
;;;; read in a matrix/vector system of polynomials describing a set of
;;;; simultaneous equations
;;;; 

(defpackage #:solve-linear-system
  (:use #:cl #:fpoly))

(in-package #:solve-linear-system)

;; load the problem
(defvar mymat (load-matrix "../examples/test-mat.dat"))

;; analyse it and generate a set of bindings and primes
(multiple-value-bind (mat-lists primes bindings) (generate-matrices mymat)
  (defvar mat-list mat-lists)
  (defvar primes primes)
  (defvar bindings bindings))

;; solve each of these using ffge
(defvar solved-mats (mapcar (lambda (mats)
							  (mapcar (lambda (mat)
										(echelon mat))
									  mats))
							mat-list))

;; interpolate them
(defvar interpolated-matrices (mapcar (lambda (mats)
										(lagrange-interpolate-matrix mats bindings 1))
									  solved-mats))

;; finally combine them using the chinese remainder theorem

(defvar solution (chinese-remainder-matrices interpolated-matrices))






									 


	  


;;;; read in a matrix/vector system of polynomials describing a set of
;;;; simultaneous equations
;;;; 

(defpackage #:solve-linear-system
  (:use #:cl #:fpoly))

(in-package #:solve-linear-system)

;; load the problem
(defvar mymat (load-matrix "../examples/test-mat.dat"))

(defvar solution (solve-system mymat))


;;;

;; generate a random system for testing purposes

(def-test-system (mymat2 myvals2) '(x y) 2 10 3 :entry-density 1 :vals '(1 2 3))

;; try and solve using simple-solve (doesn't modulo primes so uses bignums)
(simple-solve mymat)










									 


	  

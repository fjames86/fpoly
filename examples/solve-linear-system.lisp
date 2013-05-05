
;;;; read in a matrix/vector system of polynomials describing a set of
;;;; simultaneous equations
;;;; 

(defpackage #:solve-linear-system
  (:use #:cl #:fpoly))

(in-package #:solve-linear-system)

;; load the problem
(defvar mymat (load-matrix "../examples/test-mat.dat"))

(defvar solution (solve-system mymat))






									 


	  

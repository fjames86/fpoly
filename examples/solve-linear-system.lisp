
;;;; read in a matrix/vector system of polynomials describing a set of
;;;; simultaneous equations
;;;; 

(defpackage #:solve-linear-system
  (:use #:cl #:fpoly))

(in-package #:solve-linear-system)

(defvar mymat (load-matrix "../examples/test-mat.dat"))
(defvar myvec (load-vector "../examples/test-vec.dat"))




	  

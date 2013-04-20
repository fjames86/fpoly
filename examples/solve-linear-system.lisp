
;;;; read in a matrix/vector system of polynomials describing a set of
;;;; simultaneous equations
;;;; 

(defpackage #:solve-linear-system
  (:use #:cl #:fpoly))

(in-package #:solve-linear-system)




(defun solve-system (matfile vecfile)
  (let* ((mat (with-open-file (f matfile :direction :input) (load-matrix f)))
		 (vec (with-open-file (f vecfile :direction :input) (load-vector f))))
	(assert (= (mat-size mat) (array-dimension vec 0)))
	(




	  

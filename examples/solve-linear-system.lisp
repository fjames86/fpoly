
(defpackage #:solve-linear-system
  (:use #:cl #:fpoly))

(in-package #:solve-linear-system)

(defun solve-system (matfile vecfile)
  (let* ((mat (with-open-file (f matfile :direction :input) (load-matrix f)))
		 (vec (with-open-file (f vecfile :direction :input) (load-vector f))))
	(list mat vec)))




	  

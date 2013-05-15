
;;;;
;;;; COnvert polynomials to function definitions at compile time
;;;; Should be possible to "compile" the polynomial into a custom
;;;; function, rather than just calling fpoly-eval directly.
;;;; 

(in-package #:fpoly)

(defmacro defpoly (name poly)
  (let ((gp (gensym "POLY"))
		(vars (fpoly-vars poly)))
	`(let* ((,gp ,poly))
	   (defun ,name ,vars
		 (fpoly-eval ,gp (mapcar #'cons ',vars (list ,@vars)))))))


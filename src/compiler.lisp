
;;;;
;;;; Convert polynomials to function definitions at compile time
;;;; Should be possible to "compile" the polynomial into a custom
;;;; function, rather than just calling fpoly-eval directly.
;;;; 

(in-package #:fpoly)


(defun deconstruct-poly (poly)
  (let ((vars (fpoly-vars poly))
		(ret nil))
	(docoeffs (poly coeff powers)
	  (push `(* ,coeff ,@(mapcan (lambda (var power)
								   (loop for i below power collect var))
								 vars
								 powers))
			ret))
	(cons '+ ret)))

(defmacro defpoly (name poly)
  (cond
	((fpoly? poly)
	 `(defun ,name ,(fpoly-vars poly) ,(deconstruct-poly poly)))
	((symbolp poly)
	 (let ((gp (gensym "POLY")))
	   `(let ((,gp ,poly))
		  (defun ,name (&rest args)
			(fpoly-eval ,gp (mapcar #'cons
									(fpoly-vars ,gp)
									args))))))
	(t nil)))



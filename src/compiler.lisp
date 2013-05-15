
(in-package #:fpoly)

(defmacro defpoly (name poly)
  (let ((gp (gensym "POLY"))
		(vars (fpoly-vars poly)))
	`(let* ((,gp ,poly))
	   (defun ,name ,vars
		 (fpoly-eval ,gp (mapcar #'cons ',vars (list ,@vars)))))))


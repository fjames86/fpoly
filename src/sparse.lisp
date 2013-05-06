
;;;; A simple representation of sparse polynomials


(in-package #:fpoly)



(defun make-spoly ()
  "Sparse polynomials are lists of (cons coeff . powers) with powers an
alist of var/power pairs."
  nil)

(defun print-spoly (stream sp)
  "Print a sparse polynomial."
  (let (printed)
	(mapc (lambda (term)
			(let ((coeff (car term))
				  (terms (cdr term)))			  
			  (unless (zerop coeff)
				(let ((sign (if (< coeff 0) -1 1)))
				  (if printed
					  (if (= sign -1)
						  (format stream " - ")
						  (format stream " + ")))					  

				  (if (and printed (= (abs coeff) 1))
					  (format stream "~A*" (* sign coeff))
					  (format stream "~A*" coeff))
			  
				  (mapc (lambda (vp)
						  (format stream "~A^~A" (car vp) (cdr vp)))
						terms)
				  (setf printed t)))))
		  sp))
  nil)


	
						  
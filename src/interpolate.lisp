
(in-package #:fpoly)

;;; mutlivariable lagrange intepolation

(defun coeff-array (point degree)
  "Generate the coefficient array for a poly of degree at a point"
  (let ((num-vars (length point)))
	(mapcar (lambda (powers)
			  (fpoly-eval-monomial point powers))
			(gen-all-powers num-vars degree))))

(defun form-lagrange-matrix (points-list n)
  "Form a matrix of the coefficients"
  (mapcar (lambda (point)
			(coeff-array point n))
		  points-list))

(defun form-monomial (vars powers)
  "Make the monomial composed of the variables and powers"
  (let ((p (make-fpoly vars (reduce #'+ powers))))
	(setf (apply #'fpoly-coeff p powers) 1)
	p))

(defun form-monomials (vars degree)
  "List of all monomials for vars up to degree"
  (let ((nvars (length vars)))
	(mapcar (lambda (powers)
			  (form-monomial vars powers))
			(gen-all-powers nvars degree))))

(defun lagrange-interpolate (vars points vals degree)
  "Find the minimal polynomial with the degree that goes through the points with values"
  (assert (and (every (lambda (point)
						(= (length point) (length vars)))
					  points)
			   (= (length points) (length vals) (base-offset (length vars) degree))))
  (let* ((m (form-lagrange-matrix points degree))
		 (monomials (form-monomials vars degree))
		 (delta (det m))
		 (mats (loop for i below (length m) collect
					(loop for j below (length m) collect
						 (if (= j i)
							 monomials
							 (nth j m))))))
	(reduce #'fpoly-add
			(mapcar (lambda (d val)
					  (fpoly-mul (fpoly-div d delta) val))
					(mapcar #'fpoly-det mats)
					vals))))

(defun lagrange-interpolate-matrix (vars points mats degree)
  "Interpolate all the entries in the list of matrices"
  (apply #'mapcar (lambda (&rest rows)
					(apply #'mapcar (lambda (&rest entries)
									  (lagrange-interpolate vars points entries degree))
						   rows))
		 mats))

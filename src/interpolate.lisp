
;;;;
;;;; Routines for Multivariate Lagrange interpolation
;;;; Takes a set of points (variable coordinates) that go through a set of values
;;;; (values that the desired function evaluates to at these coordinates)
;;;; and returns this polynomial function
;;;;
;;;; Exposes: lagrange-interpolate, lagrange-interpolate-matrix
;;;;

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
	(reverse 
	(mapcar (lambda (powers)
			  (form-monomial vars powers))
			(gen-all-powers nvars degree)))))

;;

(defun lagrange-determinant (mat vars degree row)
  "Find the determinant of the matrix with the ith row replaced with the monomials"
  (let* ((n (car (array-dimensions mat)))
		 (m (make-array (list (1- n) (1- n)))))
	(labels ((sub-det (col)
			   (dotimes (r (1- n))
				 (dotimes (c (1- n))
				   (setf (aref m r c)
						 (aref mat
							   (if (< r row) r (1+ r))
							   (if (< c col) c (1+ c))))))
			   (format t "m(~A) : ~A~%" col m)
			   (det m)))
	  (make-fpoly vars degree
				  (reverse (loop for i below n collect
								(* (if (= row 0) 1 -1)
								   (if (= (mod i 2) 0) 1 -1)
								   (sub-det i))))))))


(defun lagrange-interpolate (vars points vals degree)
  "Find the minimal polynomial with the degree that goes through the points with values"
  (let ((n (length vars)))
	(unless (and (every (lambda (point)
						  (= (length point) n))
						points)
				 (= (length points) (length vals) (base-offset n degree)))
		(error 'fpoly-error
			   :place "LAGRANGE-INTERPOLATE"
			   :data "Number of data points does not match polynomial degree")))  
  (let* ((m (form-lagrange-matrix points degree))
		 (monomials (form-monomials vars degree))
		 (delta (det m))
		 (mats (loop for i below (length m) collect
					(loop for j below (length m) collect
						 (if (= j i)
							 monomials
							 (nth j m))))))
	(fpoly-sum (mapcar (lambda (d val)
						 (fpoly-mul (fpoly-div d delta) val))
					   (mapcar #'fpoly-det mats)
					   vals))))

(defun lagrange-interpolate-matrix (matrices bindings degree)
  "Interpolate each entry in the list of matrices."
  (let ((n (mat-size (car matrices)))
		(vars (mapcar #'car (car bindings)))
		(points (mapcar (lambda (b)
						  (mapcar #'cdr b))
						bindings)))
	(let ((mat (make-matrix n)))
	  (doentries (mat entry i j)
		(setf entry (lagrange-interpolate vars
										  points
										  (mapcar (lambda (matrix)
													(aref matrix i j))
												  matrices)
										  degree)))
	  mat)))


;;; -----------------


(defun chinese-remainder-matrices (mats primes)
  "Chinese remainder each entry in the list of polynomials."
  (let ((m (make-matrix (mat-size (car mats)))))
	(doentries (m entry i j)
	  (setf entry (fpoly-chinese-remainder (mapcar (lambda (mat)
								   (aref mat i j))
								 mats)
						 primes)))	
	m))


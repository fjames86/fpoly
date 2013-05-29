
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

(defun coeff-array (point degree &optional prime)
  "Generate the coefficient array for a poly of degree at a point"
  (let ((num-vars (length point)))
	(mapcar (lambda (powers)
			  (if prime
				  (fpoly-eval-monomial-mod point powers prime)
				  (fpoly-eval-monomial point powers)))
			(gen-all-powers num-vars degree))))

(defun form-lagrange-matrix (points degree &optional prime)
  "Create the lagrange matrix from the variable points"
  (let ((n (base-offset (length (car points)) degree)))
	(make-array (list n n)
				:initial-contents (loop for point in points collect
									   (reverse (coeff-array (reverse point) degree prime))))))

(defun lagrange-determinant (lagrange-matrix vars degree row &optional prime)
  "Find the determinant of the matrix with the row replaced with the monomials"
  (let* ((n (array-dimension lagrange-matrix 0))
		 (mat (make-array (list n n)))
		 (m (make-array (list (1- n) (1- n)))))
	(dotimes (r n)
	  (dotimes (c n)
		(cond
		  ((= r 0)
		   (setf (aref mat r c) (aref lagrange-matrix row c)))
		  ((= r row)
		   (setf (aref mat r c) (aref lagrange-matrix 0 c)))
		  (t (setf (aref mat r c) (aref lagrange-matrix r c))))))
	(labels ((sub-det (col)
			   (dotimes (r (1- n))
				 (dotimes (c (1- n))
				   (setf (aref m r c)
						 (aref mat
							   (1+ r)
							   (if (< c col) c (1+ c))))))
			   (det m prime)))
	  (make-fpoly (reverse vars) degree
				  (loop for col downfrom (1- n) to 0 collect
					   (if prime
						   (fpoly-mod (* (if (= row 0) 1 -1)
										 (if (zerop (mod col 2)) 1 -1)
										 (sub-det col))
									  prime)
						   (* (if (= row 0) 1 -1)
							  (if (zerop (mod col 2)) 1 -1)
							  (sub-det col))))))))

(defun lagrange-interpolate (vars points vals degree &optional prime)
  "Find the minimal polynomial with the degree that goes through the points with values"
  (cond
	((zerop degree)
	 (if (apply #'= vals)
		 (car vals)
		 (error 'fpoly-error
				:place "LAGRANGE-INTERPOLATE"
				:data (format nil "No solution to zero degree interpolation (all points ~A different)" vals))))
	((< degree 0)
	 (error 'fpoly-error
			:place "LAGRANGE-INTERPOLATE"
			:data "Negative degree"))
	(t
	  (let* ((n (length vars))
			 (need-n (base-offset n degree)))
		(unless (and (every (lambda (point)
							  (= (length point) n))
							points)
					 (>= (length points) need-n)
					 (>= (length vals) need-n))
		  (error 'fpoly-error
				 :place "LAGRANGE-INTERPOLATE"
				 :data "Number of data points does not match polynomial degree"))
		(let ((points (first-n points need-n))
			  (vals (first-n vals need-n)))
		  (let* ((m (form-lagrange-matrix points degree prime))
				 (delta (det m prime)))
			(if (zerop delta)
				(error 'fpoly-error
					   :place "LAGRANGE-INTERPOLATE"
					   :data (format nil "Zero determinant of lagrange matrix ~A" m)))
			(let ((deltas (loop for row below (array-dimension m 0) collect
							   (lagrange-determinant m vars degree row prime))))
			  (handler-case 
				  (fpoly-sum (mapcar (lambda (val d)
									   (if prime
										   (fpoly-mul (with-modular-arithmetic prime
														(/ val delta))
													  d)
										   (fpoly-mul (/ val delta) d)))
									 vals
									 deltas))
				(division-by-zero ()  (error 'fpoly-error
											 :place "LAGRANGE-INTERPOLATE"
											 :data "Divison by zero detetected"))))))))))

(defun lagrange-interpolate-matrix (matrices bindings degree-matrix &optional prime)
  "Interpolate each entry in the list of matrices."
  (let ((n (mat-size (car matrices)))
		(vars (mapcar #'car (car bindings)))
		(points (mapcar (lambda (b)
						  (mapcar #'cdr b))
						bindings)))
	(let ((mat (make-matrix n)))
	  (doentries (mat entry i j)
		(if (> j i)
			(fpoly-debug "Interpolating with points ~A~%"
						 (mapcar (lambda (matrix)
								   (aref matrix i j))
								 matrices)))
		(setf entry (lagrange-interpolate vars
										  points
										  (mapcar (lambda (matrix)
													(aref matrix i j))
												  matrices)
										  (aref degree-matrix i j)
										  prime)))
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


;;;;
;;;; Defines a set of functions for manipulating matrices of polynomials,
;;;; and also some other useful functions.
;;;; 

(in-package #:fpoly)

;; ---------------------------

(defun make-matrix (n)
  (make-array (list n (1+ n))))

(defun mat-size (matrix)
  (array-dimension matrix 0))

(defmacro doentries ((matrix entry-var &optional col row) &body body)
  (let ((gm (gensym "MATRIX"))
		(gi (if col col (gensym "COL")))
		(gj (if row row (gensym "ROW")))
		(gsize (gensym "SIZE")))
	`(let* ((,gm ,matrix)
			(,gsize (mat-size ,gm)))
	   (dotimes (,gi ,gsize)
		 (dotimes (,gj (1+ ,gsize))
		   (symbol-macrolet ((,entry-var (aref ,gm ,gi ,gj)))
			 ,@body)))
	   ,gm)))

(defun mapmatrix (function matrix)
  "Map a function over the entries of a matrix, returning a new matrix"
  (let ((new (make-matrix (mat-size matrix))))
	(doentries (new entry col row)
	  (setf entry (funcall function (aref matrix col row))))
	new))

;; ------ various useful functions ------------

(defun matrix-modulo (mat n)
  "Take a matrix module a number"
  (mapmatrix (lambda (p)
			   (fpoly-mod p n))
	         mat))

(defun eval-matrix (mat bindings &key prime)
  "Evaluate all the polys in the matrix using the bindings"
  (mapmatrix (lambda (p)
			   (let ((val (fpoly-eval p bindings)))
				 (if prime
					 (fpoly-mod val prime)
					 val)))
	         mat))

(defun eval-vector (vec bindings &key prime)
  "Evaluate each entry in the vector"
  (make-array (array-dimension vec 0)
			  :initial-contents
			  (loop for i below (array-dimension vec 0)
				   collect (let ((val (fpoly-eval x bindings)))
							 (if prime
								 (fpoly-mod val prime)
								 val)))))

(let ((prime-list (primes 1000)))
  (defun choose-primes (mat &key (lowest-prime 5))
	"Choose the smallest n primes such that their product
is >= 2*x where x is the largest coefficient in the matrix provided"
	(let ((x (let (max-coeff)
			   (doentries (mat entry)
				 (let ((c (if (numberp entry)
							  entry
							  (apply #'max (coerce (fpoly-coeffs entry) 'list)))))
				   (cond
					 ((null max-coeff)
					  (setf max-coeff c))
					 ((> c max-coeff)
					  (setf max-coeff c)))))
			   max-coeff))
		  (primes (drop-until (lambda (n)
								(>= n lowest-prime))
							  prime-list)))
	  (do ((prod 1)
		   (pms nil))
		  ((or (>= prod (* 2 x))
			   (null primes))
		   (nreverse pms))
		(let ((p (pop primes)))
		  (setf prod (* prod p))
		  (push p pms))))))

(defun choose-binding (vars prime)
  (mapcar (lambda (var)
			(cons var (- (random (* 2 prime)) prime)))
		  vars))

(defun choose-bindings (mat &key (degree 1) (prime 5))
  "Given a matrix, compute a set of bindings for the symbols"
  (let ((vars (let (vars)
				(doentries (mat entry)
				  (if (fpoly? entry)
					  (mapcar (lambda (var)
								(pushnew var vars))
							  (fpoly-vars entry))))
				vars)))
	(loop for i below (base-offset (length vars) degree) collect 
		 (mapcar (lambda (var)
				   (cons var (- (random (* prime 2)) prime)))
				 vars))))

(defun find-max-degree (mat)
  "Sum the degree of the diagonal elements of the matrix."
  (let ((max-degree 0))
	(dotimes (i (mat-size mat))
	  (let ((entry (aref mat i i)))
		(let ((degree (if (fpoly? entry)
						  (fpoly-degree entry)
						  0)))
		  (incf max-degree degree))))
	max-degree))

(defun solve-matrix (mat prime)
  "Choose some bindings, evaluate at each binding, solve using ffge and recombine back
into a solution matrix."
  (let ((max-degree (find-max-degree mat)))
	(let ((binding-list (choose-bindings mat
										 :degree max-degree
										 :prime prime)))
	  (let ((ms (mapcar (lambda (bindings)
						  (echelon (eval-matrix mat
												bindings
												:prime prime)))
						binding-list)))
		(lagrange-interpolate-matrix ms
									 binding-list
									 max-degree)))))


(defun combine-matrices (mat-list primes)
  "Chinese remainder each entry of the matrices to form a matrix of combined entries."
  (let ((m (make-matrix (mat-size (car mat-list)))))
	(doentries (m entry i j)
	  (setf entry (fpoly-chinese-remainder (mapcar (lambda (mat)
													 (aref mat i j))
												   mat-list)
										   primes)))
	m))

(defun solve-system (mat)
  (let ((primes (choose-primes mat)))
	(combine-matrices (mapcar (lambda (prime)
								(solve-matrix (matrix-modulo mat prime) prime))
							  primes)
					  primes)))

;; --------------------- printers ---------------------

(defun print-matrices (fname mats)
  "Print out a list of matrices. typically matrices of numbers only"
  (with-open-file (f fname :direction :output :if-exists :supersede)
	(mapc (lambda (mat)
			(doentries (mat entry)
			  (format f "~A " entry))
			(format f "~%"))
		  mats))
  'ok)

(defun print-matrix-mma (stream matrix)
  "Print a matrix in mathematica format, {{poly, ...}, ...}"
  (princ "{" stream)
  (dotimes (i (mat-size matrix))
	(if (> i 0) (princ ", " stream))
	
	(princ "{" stream)
	(dotimes (j (1+ (mat-size matrix)))
	  (if (> j 0) (princ ", " stream))
	  (princ (aref matrix i j) stream))
	(princ "}" stream))
  
  (princ "}" stream)
  nil)

;; ------------------- matrix reduction and inversion -----------------------------------

(defun pivot (mat i n)
  "Find the first row >= i with element (i,j) non-zero, then swap the rows"
  (labels ((rec (row)
			 (cond
			   ((= row n) nil) ; failed to swap any rows
			   ((zerop (aref mat row i))
				(rec (1+ row)))
			   (t (dotimes (col (1+ n))
					(rotatef (aref mat i col) (aref mat row col)))
				  t))))
	(rec i)))


(defun echelon (a)
  "Reduce a matrix of numbers to row-echelon form,
using the fraction free Gaussian Eliminaton alg"
	(let ((n (car (array-dimensions a))))
	  (dotimes (i (1- n))
		;; pivot if needed
		(if (zerop (aref a i i))
			(if (not (pivot a i n))
				(error 'fpoly-error
					   :place "FFGE"
					   :data (format nil
									 "Unsolveable matrix ~A, all zeroes in pivot column"
									 a))))
		
		(loop for j from (1+ i) to (1- n) do
			 (setf (aref a j n) (- (* (aref a i i) (aref a j n))
								   (* (aref a j i) (aref a i n))))
			 (if (> i 0) 
				 (multiple-value-bind (q r) (truncate (aref a j n) (aref a (1- i) (1- i)))
				   (declare (ignore r)) ; should be zero
				   (setf (aref a j n) q)))
			 (loop for k from (1+ i) to (1- n) do
				  (setf (aref a j k) (- (* (aref a i i) (aref a j k))
										(* (aref a j i) (aref a i k))))
				  (if (> i 0)
					  (multiple-value-bind (q r) (truncate (aref a j k)
														   (aref a (1- i) (1- i)))
						(declare (ignore r))
						(setf (aref a j k) q))))
			 (setf (aref a j i) 0))))
	a)

(defun make-identity (n)
  "Make an identity matrix"
  (let ((mat (make-array (list n n) :initial-element 0)))
	(dotimes (i n)
	  (setf (aref mat i i) 1))
	mat))

(defun invert (matrix)
  "Invert a matrix of numbers using gauss jordan elimination"
  (unless (= (array-dimension matrix 0) (array-dimension matrix 1))
	(error "Not a square matrix."))

  (let* ((n (array-dimension matrix 0))
		 (a (copy-array matrix))
		 (b (make-identity n)))
	(labels ((add-rows (mat r1 r2 factor)
			   (dotimes (i n)
				 (setf (aref mat r1 i)
					   (+ (aref mat r1 i)
						  (* factor (aref mat r2 i)))))
			   mat)
			 (scale-row (mat r factor)
			   (dotimes (i n)
				 (setf (aref mat r i)
					   (* factor (aref mat r i))))
			   mat)
			 (swap-row (mat r1 r2)
			   (dotimes (i n)
				 (rotatef (aref mat r1 i) (aref mat r2 i)))
			   mat))
	  (dotimes (row n)
		(dotimes (col n)
		  (cond
			((> (abs (aref a col row)) (abs (aref a row row)))
			 (swap-row a row col)
			 (swap-row b row col))))
		(let ((s (aref a row row)))
		  (cond
			((zerop s) (error "Non-invertible matrix."))
			(t
			 (scale-row a row (/ s))
			 (scale-row b row (/ s)))))
		(dotimes (i n)
		  (unless (= i row)
			(let ((s (- (aref a i row))))
			  (add-rows a i row s)
			  (add-rows b i row s)))))
	  b)))

;; ----------------------

(defun mat-list (mat)
  "Convert a matrix to nested lists."
  (let ((n (mat-size mat)))
	(loop for i below n collect
		 (loop for j below n collect
			  (aref mat i j)))))

(defun list-mat (mlist)
  "Convert nested lists to an array."
  (let ((n (length mlist)))
	(make-array (list n n)
				:initial-contents mlist)))

(defun fpoly-det (mat)
  "Compute the determinant of a matrix of polys."
  (format *error-output* "Warning: fpoly-det is incorrect and very slow. don't use it!")
  (labels ((sub-det (terms)
			 (let ((n (length terms)))
			   (cond
				 ((= n 1)
				  (car terms))
				 ((= n 2)
				  (destructuring-bind ((x11 x12) (x21 x22)) terms
					(fpoly-sub (fpoly-mul x11 x22) (fpoly-mul x12 x21))))
				 (t
				  (do ((s 1 (- s))
					   (i 0 (1+ i))
					   (row (car terms) (cdr row))
					   (sum 0
							(fpoly-add sum
									  (fpoly-mul s
												(fpoly-mul (car row)
														  (sub-det (sub-mat terms i 0)))))))
					  ((null row) sum)))))))
	(sub-det (if (arrayp mat)
				 (mat-list mat)
				 mat))))

;;; --------------------------------------------------------

;;; lu decomposition and determinant follows
;;; taken from  http://rosettacode.org/wiki/LU_decomposition#Common_Lisp
;;;
 
(defun mmul (A B)
  (let* ((m (car (array-dimensions A)))
		 (n (cadr (array-dimensions A)))
		 (l (cadr (array-dimensions B)))
		 (C (make-array `(,m ,l) :initial-element 0)))
	(loop for i from 0 to (- m 1) do
		 (loop for k from 0 to (- l 1) do
			  (setf (aref C i k)
					(loop for j from 0 to (- n 1)
					   sum (* (aref A i j)
							  (aref B j k))))))
	C))

;; Creates a nxn identity matrix.
(defun eye (n)
  (let ((I (make-array `(,n ,n) :initial-element 0)))
	(loop for j from 0 to (- n 1) do
		 (setf (aref I j j) 1))
	I))

;; Swap two rows l and k of a mxn matrix A, which is a 2D array.
(defun swap-rows (A l k)
  (let* ((n (cadr (array-dimensions A)))
		 (row (make-array n :initial-element 0)))
	(loop for j from 0 to (- n 1) do
		 (setf (aref row j) (aref A l j))
		 (setf (aref A l j) (aref A k j))
		 (setf (aref A k j) (aref row j)))))

;; Creates the pivoting matrix for A.
(defun pivotize (A)
  (let* ((n (car (array-dimensions A)))
		 (P (eye n))
		 (s 0))
	(loop for j from 0 to (- n 1) do
		 (let ((max (aref A j j))
			   (row j))
		   (loop for i from j to (- n 1) do
				(if (> (aref A i j) max)
					(setq max (aref A i j)
						  row i)))
		   (if (not (= j row))
			   (progn
				 (swap-rows P j row)
				 (incf s)))))

	;; Return P.
	(values P s)))

;; Decomposes a square matrix A by PA=LU and returns L, U and P.
(defun lu (A)
  (let* ((n (car (array-dimensions A)))
		 (L (make-array `(,n ,n) :initial-element 0))
		 (U (make-array `(,n ,n) :initial-element 0)))
	(multiple-value-bind (P s) (pivotize A)
	  (let ((A (mmul P A)))

	(loop for j from 0 to (- n 1) do
		 (setf (aref L j j) 1)
		 (loop for i from 0 to j do
			  (setf (aref U i j)
					(- (aref A i j)
					   (loop for k from 0 to (- i 1)
						  sum (* (aref U k j)
								 (aref L i k))))))
		 (loop for i from j to (- n 1) do
			  (setf (aref L i j)
					(/ (- (aref A i j)
						  (loop for k from 0 to (- j 1)
							 sum (* (aref U k j)
									(aref L i k))))
					   (aref U j j)))))

	;; Return L, U and P.
	(values L U P s)))))

(defun det (mat)
  "Find the determinant of an n x n matrix using LU decomposition."
  (let ((n (car (array-dimensions mat))))
	(multiple-value-bind (l u p s) (lu mat)
	  (declare (ignore p))
	  (let ((d (expt -1 s)))
		(dotimes (i n)
		  (setf d (* d (aref l i i) (aref u i i))))
		d))))

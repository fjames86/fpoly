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
			   ((= row n)
				(error 'fpoly-error
					   :place "PIVOT"
					   :data (format nil
									 "Unsolveable matrix ~A, all zeroes in pivot column"
									 mat)))
			   ((zerop (aref mat row i))
				(rec (1+ row)))
			   (t (dotimes (col (1+ n))
					(rotatef (aref mat i col) (aref mat row col)))))))
	(rec i)))


(defun echelon (a)
  "Reduce a matrix of numbers to row-echelon form,
using the fraction free Gaussian Eliminaton alg."
	(let ((n (car (array-dimensions a)))
		  (swaps 0)
		  (muls 1))
	  (dotimes (i (1- n))
		;; pivot if needed
		(if (zerop (aref a i i))
			(progn
			  (pivot a i n)
			  (incf swaps)))
		
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
			 (setf (aref a j i) 0))
		(setf muls (* muls (aref a i i)))
		(if (> i 0) (setf muls (/ muls (aref a (1- i) (1- i))))))
	  (values a swaps muls)))

(defun make-pivot (matrix)
  (let ((n (car (array-dimensions matrix))))
	(let ((p (make-identity n)))
	  (dotimes (i n)
		(if (zerop (aref matrix i i))
			(let (row)
			  (loop for j from (1+ i) to (1- n) do
				   (if (not (zerop (aref matrix j i)))
					   (setf row j)))
			  (if row
				  (swap-rows p i row)
				  (error 'fpoly-error
						 :place "MAKE-PIVOT"
						 :data "Unable to pivot matrix")))))
	  p)))

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

;;; --------------------------------------------------------

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

;;; ----------------------

(defun lu-test (matrix)
  (let ((n (array-dimension matrix 0)))
	(multiple-value-bind (u l p dd) (lu-decomposition matrix)
	  (format t "u: ~A~%l: ~A~%p: ~A~%dd: ~A~%" u l p dd)
	  (let ((d (make-array (list n n))))
		(dotimes (i n)
		  (setf (aref d i i) (svref dd i)))
		(values (mmul p matrix)
				(mmul l (mmul (invert d) u)))))))

(defun lu-decomposition (matrix)
  (let* ((n (array-dimension matrix 0))
		 (u (copy-array matrix))
		 (l (make-identity n))
		 (p (make-identity n))
		 (dd (make-array n))
		 (oldpivot 1)
		 (nswaps 0))
	(dotimes (k (1- n))
	  (if (zerop (aref u k k))
		  (let ((kpivot (1+ k))
				(notfound t))
			(loop while (and (< kpivot n) notfound) do
				 (if (not (zerop (aref u kpivot k)))
					 (setf notfound nil)
					 (incf kpivot)))
			(if (= kpivot n)
				(error "cant pivot matrix")
				(loop for col from k to (1- n) do
					 (progn
					   (rotatef (aref u k col) (aref u kpivot col))
					   (rotatef (aref p k col) (aref p kpivot col)))))
			(incf nswaps)))
	  (setf (aref l k k) (aref u k k)
			(svref dd k) (* oldpivot (aref u k k)))
	  (loop for i from (1+ k) to (1- n) do
		   (progn
			 (setf (aref l i k) (aref u i k))
			 (loop for j from (1+ k) to (1- n) do
				  (multiple-value-bind (q r) (truncate (- (* (aref u k k) (aref u i j))
														  (* (aref u k j) (aref u i k)))
													   oldpivot)
					(unless (zerop r)
					  (error "remainder non zero: ~A" r))
					(setf (aref u i j) q)))
			 (setf (aref u i k) 0)))
	  (setf oldpivot (aref u k k)))
	(setf (svref dd (1- n)) oldpivot)
	(values u l p dd nswaps)))

(defun lu-det (matrix)
  (let ((n (array-dimension matrix 0)))
	(multiple-value-bind (u l p dd nswaps) (lu-decomposition matrix)
	  (declare (ignore p)) ;; this gives the sign,
	  (let ((det (if (zerop (mod nswaps 2)) 1 -1)))
		(dotimes (i n)
		  (setf det (* det (aref u i i) (aref l i i)))
		  (setf det (/ det (svref dd i))))
		det))))
	  
(defun det (matrix)
  (let ((n (array-dimension matrix 0)))
	(cond
	  ((= n 2)
	   (- (* (aref matrix 0 0) (aref matrix 1 1))
		  (* (aref matrix 0 1) (aref matrix 1 0))))
	  (t (lu-det matrix)))))



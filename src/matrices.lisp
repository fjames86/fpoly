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

(defmacro doentries ((matrix entry-var &optional row col) &body body)
  (let ((gm (gensym "MATRIX"))
		(grow (if row row (gensym "ROW")))
		(gcol (if col col (gensym "COL")))
		(gsize (gensym "SIZE")))
	`(let* ((,gm ,matrix)
			(,gsize (mat-size ,gm)))
	   (dotimes (,grow ,gsize)
		 (dotimes (,gcol (1+ ,gsize))
		   (symbol-macrolet ((,entry-var (aref ,gm ,grow ,gcol)))
			 ,@body)))
	   ,gm)))

(defun mapmatrix (function matrix)
  "Map a function over the entries of a matrix, returning a new matrix"
  (let ((new (make-matrix (mat-size matrix))))
	(doentries (new entry row col)
	  (setf entry (funcall function (aref matrix row col))))
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
			   (if prime
				   (fpoly-eval-mod p bindings prime)
				   (fpoly-eval p bindings)))			 
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
	(let ((x (max-coeff-matrix mat))
		  (primes (drop-until (lambda (n)
								(>= n lowest-prime))
							  prime-list)))
	  (fpoly-debug "max coeff: ~A~%" x)
	  (do ((prod 1)
		   (pms nil))
		  ((or (>= prod (* 2 x))
			   (null primes))
		   (nreverse pms))
		(let ((p (pop primes)))
		  (setf prod (* prod p))
		  (push p pms))))))

(defun not-in (bindings forbidden-bindings)
  (every (lambda (fbinding)
		   (not (every (lambda (b)
						 (destructuring-bind (var . val) b
						   (= val (cdr (assoc var fbinding)))))
					   bindings)))
		 forbidden-bindings))

(defun num-possible-bindings (nvars prime)
  (let ((n (1+ (* 2 (1- prime)))))
	(expt n nvars)))

(defun binding-density (nvars degree prime)
  (/ (base-offset nvars degree)
	 (num-possible-bindings nvars prime)))


(defun next-binding (current-bindings prime)
  (labels ((rec (bs)
			 (if (null bs)
				 (error 'fpoly-error
						:place "NEXT-BINDING"
						:data "Out of possible bindings. Try a larger prime!")
				 (destructuring-bind (var . b) (car bs)
				   (let ((b1 (1+ b)))
					 (if (= b1 prime)
						 (cons (cons var (1+ (- prime)))
							   (rec (cdr bs)))
						 (cons (cons var b1)
							   (cdr bs))))))))
	(rec current-bindings)))
  
(defun choose-binding (vars polys prime forbidden-bindings &key (max-attempts 100))
  (do ((bindings 
		(mapcar (lambda (var)
				  (cons var (fpoly-mod (random (* 2 prime)) prime)))
				  vars)
		(mapcar (lambda (var)
				  (cons var (fpoly-mod (random (* 2 prime)) prime)))
				vars))
	   (counter 0 (1+ counter)))
	  ((and (every (lambda (p)
					 (not (zerop (fpoly-eval-mod p bindings prime))))
				   polys)
			(not-in bindings forbidden-bindings)
			(< counter max-attempts))
	   bindings)
	(if (= counter max-attempts)
		(error 'fpoly-error
			   :place "CHOOSE-BINDING"
			   :data "Exceeded max attempts at finding some bindings. Suggest increasing minimum prime."))))
	

(defun choose-bindings (mat &key (degree 1) (prime 5))
  "Given a matrix, compute a set of bindings for the symbols"
  (let (vars polys)
	(doentries (mat entry)
	  (if (fpoly? entry)
		  (progn
			(mapc (lambda (var)
					(pushnew var vars))
				  (fpoly-vars entry))
			(push entry polys))))

	(let ((n (base-offset (length vars) degree)))
	  (do ((i 0 (1+ i))
		   (bindings nil (cons (choose-binding vars polys prime bindings)
							   bindings)))
		  ((= i n) bindings)))))


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
  (let ((degree-matrix (degree-matrix mat))
		(max-degree 0))
	(doentries (degree-matrix entry)
	  (if (> entry max-degree)
		  (setf max-degree entry)))

	(let ((binding-list (choose-bindings mat
										 :degree max-degree
										 :prime prime)))
	  (fpoly-debug "Prime: ~A Chose bindings ~A~%" prime binding-list)
	  (let ((evaled-mats (mapcar (lambda (bindings)
								   (eval-matrix mat
												bindings
												:prime prime))
								 binding-list)))
		(let ((ms (mapcar (lambda (evaled-mat)
;							(echelon evaled-mat prime))
							(matrix-modulo (echelon evaled-mat) prime))
						  evaled-mats)))
		  (lagrange-interpolate-matrix ms
									   binding-list
									   degree-matrix
									   prime))))))


(defun combine-matrices (mat-list primes)
  "Chinese remainder each entry of the matrices to form a matrix of combined entries."
  (let ((m (make-matrix (mat-size (car mat-list)))))
	(doentries (m entry row col)
	  (setf entry (fpoly-chinese-remainder (mapcar (lambda (mat)
													 (aref mat row col))
												   mat-list)
										   primes)))
	m))

(defun solve-system (mat &key (try-count 10) (lowest-prime 5))
  (labels ((try-solve ()
			 (let ((primes (choose-primes mat :lowest-prime lowest-prime)))
			   (combine-matrices (mapcar (lambda (prime)
										   (let ((i 0)
												 (notdone t)
												 (m nil))
											 (loop while (and notdone (< i try-count)) do
												  (handler-case
													  (progn
														(incf i)
														(setf m (solve-matrix (matrix-modulo mat prime) prime))
														(setf notdone nil))
													(fpoly-error (err)
													  (fpoly-debug "Error ~A at ~A~%"
																   (fpoly-error-data err)
																   (fpoly-error-place err)))))
											 (if m
												 m
												 (error 'fpoly-error
														:place "SOLVE-SYSTEM:TRY-SOLVE"
														:data "Try count exceeded"))))
										 primes)
								 primes))))
	(fpoly-debug "Attempting to solve with ~A attempts remaining...~%" try-count)
	(if (zerop try-count)
		nil
		(handler-case (try-solve)
		  (fpoly-error (err)
			(fpoly-debug "Failed at ~A with error ~A~%"
						 (fpoly-error-place err)
						 (fpoly-error-data err))
			(solve-system mat
						  :try-count (1- try-count)
						  :lowest-prime lowest-prime))))))
						  


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
  (let ((row (1+ i))
		(canpivot t))
	(loop while (and (< row n) canpivot) do
		 (if (zerop (aref mat row i))
			 (incf row)
			 (setf canpivot nil)))
	(if (= row n)
		(error 'fpoly-error
			   :place "PIVOT"
			   :data (format nil "Unsolveable matrix ~A all zeroes in pivot column ~A~%" mat i))
		(swap-rows mat i row))))
	
(defun swap-rows (mat r1 r2)
  "Destructively swaps elements in rows r1 and r2"
  (let ((n (array-dimension mat 0)))
	(dotimes (col n)
	  (rotatef (aref mat r1 col) (aref mat r2 col)))
	mat))

(defun echelon? (matrix)
  "Check to see if already in echelon form."
  (let ((n (array-dimension matrix 0)))
	(let ((solved t))
	  (dotimes (col n)
		(let ((row (1+ col)))
		  (loop while (and (< row n) solved) do
			   (if (not (zerop (aref matrix row col)))
				   (setf solved nil)
				   (incf row)))))
	  solved)))

(defun echelon (a &optional prime)
  "Reduce a matrix of numbers to row-echelon form,
using the fraction free Gaussian Eliminaton algorithm."
  (if (echelon? a)
	  (values a 0 1)
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
			   (with-modular-arithmetic prime
				 (setf (aref a j n) (- (* (aref a i i) (aref a j n))
									   (* (aref a j i) (aref a i n)))))
			   (if (> i 0) 
				   (multiple-value-bind (q r) (with-modular-arithmetic prime
												(truncate (aref a j n) (aref a (1- i) (1- i))))
					 (declare (ignore r)) ; should be zero
					 (setf (aref a j n) q)))
			   (loop for k from (1+ i) to (1- n) do
					(with-modular-arithmetic prime
					  (setf (aref a j k) (- (* (aref a i i) (aref a j k))
											(* (aref a j i) (aref a i k)))))
					(if (> i 0)
						(multiple-value-bind (q r) (with-modular-arithmetic prime
													 (truncate (aref a j k)
															   (aref a (1- i) (1- i))))
						  (declare (ignore r))
						  (setf (aref a j k) q))))
			   (setf (aref a j i) 0))
		  (with-modular-arithmetic prime
			(setf muls (* muls (aref a i i)))
			(if (> i 0) (setf muls (/ muls (aref a (1- i) (1- i)))))))
		(values a swaps muls))))

(defun echelo (matrix &optional prime)
  "Consing version of echelon"
  (let ((a (copy-array matrix)))
	(echelon a prime)))


;; ----------

(defun deg-add (a b)
  (max a b))

(defun deg-sub (a b)
  (max a b))

(defun deg-mul (a b)
  (+ a b))

(defun deg-div (a b)
  (- a b))

(defun degree-matrix (matrix)
  "Find the degree of resulting polynomials after ffge operation"
	(let* ((n (array-dimension matrix 0))
		   (a (make-matrix n)))		   
	  ;; setup
	  (dotimes (i n)
		(dotimes (j (1+ n))
		  (setf (aref a i j) (fpoly-degree (aref matrix i j)))))

	  ;; assume already pivoted
	  (dotimes (i (1- n))
		
		(loop for j from (1+ i) to (1- n) do
			 (setf (aref a j n) (deg-sub (deg-mul (aref a i i) (aref a j n))
										 (deg-mul (aref a j i) (aref a i n))))
			 (if (> i 0) 
				 (setf (aref a j n) (deg-div (aref a j n) (aref a (1- i) (1- i)))))
			 (loop for k from (1+ i) to (1- n) do
				  (setf (aref a j k) (deg-sub (deg-mul (aref a i i) (aref a j k))
											  (deg-mul (aref a j i) (aref a i k))))
				  (if (> i 0)
					  (setf (aref a j k) (deg-div (aref a j k) (aref a (1- i) (1- i))))))
			 (setf (aref a j i) 0)))
	  a))

(defun max-coeff (poly)
  (if (fpoly? poly)
	  (let ((coeffs (fpoly-coeffs poly))
			(max nil))
		(dotimes (i (fpoly-size poly))
		  (cond
			((null max) (setf max (svref coeffs i)))
			((> (abs (svref coeffs i)) max) (setf max (abs (svref coeffs i))))))
		max)
	  poly))

(defun max-coeff-matrix (matrix)
  "Find the maximum coefficient of resulting polynomials after ffge operation"
	(let* ((n (array-dimension matrix 0))
		   (a (make-matrix n)))
	  
	  ;; setup
	  (dotimes (i n)
		(dotimes (j (1+ n))
		  (setf (aref a i j) (max-coeff (aref matrix i j)))))

	  ;; assume already pivoted
	  (dotimes (i (1- n))
		
		(loop for j from (1+ i) to (1- n) do
			 (setf (aref a j n) (max (* (aref a i i) (aref a j n))
									 (* (aref a j i) (aref a i n))))
;			 (if (> i 0) 
;				 (setf (aref a j n) (/ (aref a j n) (aref a (1- i) (1- i)))))
			 (loop for k from (1+ i) to (1- n) do
				  (setf (aref a j k) (max (* (aref a i i) (aref a j k))
										  (* (aref a j i) (aref a i k)))))
;				  (if (> i 0)
;					  (setf (aref a j k) (/ (aref a j k) (aref a (1- i) (1- i))))))
			 (setf (aref a j i) 0)))

	  ;; return result
	  (let (max)
		(doentries (a entry)
		  (cond
			((null max) (setf max entry))
			((> entry max) (setf max entry))))
		max)))

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
	(error 'fpoly-error
		   :place "INVERT"
		   :data "Not a square matrix."))

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
			((zerop s) (error 'fpoly-error
							  :place "INVERT"
							  :data "Non-invertible matrix."))
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
	(multiple-value-bind (u l p dd) (lu-decompose matrix)
	  (format t "u: ~A~%l: ~A~%p: ~A~%dd: ~A~%" u l p dd)
	  (let ((d (make-array (list n n))))
		(dotimes (i n)
		  (setf (aref d i i) (svref dd i)))
		(values (mmul p matrix)
				(mmul l (mmul (invert d) u)))))))

(defun lu-decompose (matrix &optional prime)
  "Decompose a SQUARE matrix into u l p dd nswaps values"
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
				(error 'fpoly-error
					   :place "LU-DECOMPOSE"
					   :data (format nil "Unable to pivot column ~A. matrix:~% ~A~%" k matrix))
				(loop for col from k to (1- n) do
					 (progn
					   (rotatef (aref u k col) (aref u kpivot col))
					   (rotatef (aref p k col) (aref p kpivot col)))))
			(incf nswaps)))
	  (setf (aref l k k) (aref u k k)
			(svref dd k) (with-modular-arithmetic prime
						   (* oldpivot (aref u k k))))
	  (loop for i from (1+ k) to (1- n) do
		   (progn
			 (setf (aref l i k) (aref u i k))
			 (loop for j from (1+ k) to (1- n) do
				  (multiple-value-bind (q r) (with-modular-arithmetic prime
											   (truncate (- (* (aref u k k) (aref u i j))
															(* (aref u k j) (aref u i k)))
														 oldpivot))
					(unless (zerop r)
					  (error 'fpoly-error
							 :place "LU-DECOMPOSE"
							 :data "Remainder non zero: ~A" r))
					(setf (aref u i j) q)))
			 (setf (aref u i k) 0)))
	  (setf oldpivot (aref u k k)))
	(setf (svref dd (1- n)) oldpivot)
	(values u l p dd nswaps)))

(defun lu-det (matrix &optional prime)
  "Compute matrix determinant using lu-decompose"
  (let ((n (array-dimension matrix 0)))
	(multiple-value-bind (u l p dd nswaps) (lu-decompose matrix prime)
	  (declare (ignore p)) ;; pivot matrix, don't need it
	  (let ((det (if (zerop (mod nswaps 2)) 1 -1)))
		(with-modular-arithmetic prime
		  (dotimes (i n)
			(setf det (* det (aref u i i)))
			(setf det (* det (aref l i i)))
			(setf det (/ det (svref dd i)))))
		det))))
	  
(defun det (matrix &optional prime)
  "Matrix determinant"
  (let ((n (array-dimension matrix 0)))
	(cond
	  ((= n 2)
	   (with-modular-arithmetic prime
		 (- (* (aref matrix 0 0) (aref matrix 1 1))
			(* (aref matrix 0 1) (aref matrix 1 0)))))
	  (t (lu-det matrix prime)))))


;;;;


(defun random-poly (vars max-degree max-coeff &key (coeff-density 1.0))
  (let ((p (make-fpoly vars max-degree)))
	(docoeffs (p coeff powers)
	  (if (< (random 1.0) coeff-density)
		  (setf coeff (fpoly-mod (random (* 2 max-coeff)) max-coeff))
		  (setf coeff 0)))
	p))

(defun gen-test-system (vars max-degree max-coeff n &key
						(entry-density 0.5) (coeff-density 0.3))
  "Generate a random n x n system with polynomials in up to nvars
of max degree with max coeffs."
  (let ((m (make-matrix n))
		(varvals (loop for i below n collect (random max-coeff))))
	(dotimes (row n)
	  (dotimes (col n)
		(if (< (random 1.0) entry-density)
			(setf (aref m row col) (random-poly vars max-degree max-coeff
												:coeff-density coeff-density))
			(setf (aref m row col) 0))))

	;; now set the values on the n-th column
	(dotimes (row n)
	  (setf (aref m row n) (fpoly-sum (loop for i below n collect
										   (fpoly-mul (nth i varvals) (aref m row i))))))

	(values m varvals)))

(defmacro def-test-system (name vals)
  `(multiple-value-bind (name vals) (gen-test-system '(x y) 2 10 3)
	 (defparameter ,name name)
	 (defparameter ,vals vals)))


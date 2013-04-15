
(in-package #:fpoly)

;; ---------------------------

(defclass matrix ()
  ((entries :reader mat-entries :writer set-mat-entries :initarg :entries)
   (size :reader mat-size :initarg :n)))

(defun make-matrix (n &optional entries)
  (let ((m (make-instance 'matrix
						  :n n
						  :entries (cond
									 ((arrayp entries) entries)
									 ((null entries)
									  (make-array (list n n)
												  :initial-element 0))
									 ((listp entries)
									  (make-array (list n n)
												  :initial-contents entries))
									 (t (make-array (list n n) :initial-contents 0))))))
	m))

(defun mat-entry (matrix i j)
  (aref (mat-entries matrix) i j))

(defun (setf mat-entry) (val matrix i j)
  (let ((e (mat-entries matrix)))
	(setf (aref e i j) val)
	(set-mat-entries matrix e)
	matrix))

(defmacro doentries ((matrix entry-var &optional col row) &body body)
  (let ((gm (gensym "MATRIX"))
		(gi (if col col (gensym "COL")))
		(gj (if row row (gensym "ROW")))
		(gentries (gensym "ENTRIES"))
		(gsize (gensym "SIZE")))
	`(let* ((,gm ,matrix)
			(,gsize (mat-size ,gm))
			(,gentries (mat-entries ,gm)))
	   (dotimes (,gi ,gsize)
		 (dotimes (,gj ,gsize)
		   (symbol-macrolet ((,entry-var (aref ,gentries ,gi ,gj)))
			 ,@body)))
	   ,gm)))


(defun mapmatrix (function matrix)
  (let ((new (make-matrix (mat-size matrix))))
	(doentries (new entry col row)
	  (setf entry (funcall function (mat-entry matrix col row))))	
	new))

(defmethod print-object ((mat matrix) stream)
  (if *print-escape*
      (print-unreadable-object (mat stream :type t)
		(format stream ":SIZE ~A :ENTRIES ~A"
				(mat-size mat)
				(mat-entries mat)))	  
	  (progn
		(format stream "c r  entry~%")
		(doentries (mat entry col row)
		  (format stream "~A ~A: ~A~%" col row entry))
		(format stream "~%"))))
	  

										  
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
					  (setf max-coeff c)))))))
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

(defun choose-bindings (mat &key (degree 1) (mod-prime 5))
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
				   (cons var (- (random (* mod-prime 2)) mod-prime)))
				 vars))))

(defun generate-matrices (mat)
  "Given a matrix, compute it modulo several primes,
then evaluate each of these at some points"
  (let ((primes (choose-primes mat))
		(bindings (choose-bindings mat)))
	(values
	 (mapcar (lambda (prime)
			   (eval-matrix (matrix-modulo mat prime)
							bindings
							:prime prime))
			 primes)
	 primes
	 bindings)))

(defun print-matrices (fname mats)
  "Print out a list of matrices. typically matrices of numbers only"
  (with-open-file (f fname :direction :output :if-exists :supersede)
	(mapc (lambda (mat)
			(doentries (mat entry)
			  (format f "~A " entry))
			(format f "~%"))
		  mats))
  'ok)

;; -----------------------------------------------
;; don't need these anymore, keep them in for the moment but don't export

(defun make-mat-arrays (mat)
  "Convert a list format matrix into arrays"
  (let (a b)
	(do ((rows mat (cdr rows)))
		((null rows))
	  (push nil a)
	  (do ((row (car rows) (cdr row)))
		  ((null row))
		(if (null (cdr row))
			(setf b (append b (list (car row))))
			(setf (car a) (append (car a) (list (car row)))))))
	(let ((n (length b)))
	  (values (make-array (list n n) :initial-contents (nreverse a))
			  (make-array n :initial-contents b)))))

(defun make-mat-from-arrays (a b)
  "Convert arrays into a list format matrix"
  (let ((n (array-dimension b 0))
		(mat nil))
	(do ((i 0 (1+ i)))
		((= i n))
	  (do ((j 0 (1+ j))
		   (row nil))
		  ((= j n) (push (nreverse (cons (svref b i) row)) mat))
		(push (aref a i j) row)))
	(nreverse mat)))
;; ------------------------------------------------------

(defun ffge (mat vec)
  "Reduce a matrix of numbers to row-echelon form,
using the fraction free Gaussian Eliminaton alg"
  (let ((a (copy-array mat))
		(b (copy-array vec)))
	(let ((n (array-dimension b 0)))
	  (dotimes (i (1- n))
		(loop for j from (1+ i) to (1- n) do
			 (setf (svref b j) (- (* (aref a i i) (svref b j))
								  (* (aref a j i) (svref b i))))
			 (if (> i 0) 
				 (multiple-value-bind (q r) (truncate (svref b j) (aref a (1- i) (1- i)))
				   (declare (ignore r))
				   (setf (svref b j) q)))
			 (loop for k from (1+ i) to (1- n) do
				  (setf (aref a j k) (- (* (aref a i i) (aref a j k))
										(* (aref a j i) (aref a i k))))
				  (if (> i 0)
					  (multiple-value-bind (q r) (truncate (aref a j k) (aref a (1- i) (1- i)))
						(declare (ignore r))
						(setf (aref a j k) q))))
			 (setf (aref a j i) 0))))
	(list a b)))


;; ----------------------

(defun mat-list (mat)
  "Convert a matrix to nested lists"
  (let ((n (array-dimension mat 0)))
	(loop for i below n collect
		 (loop for j below n collect
			  (aref mat i j)))))

(defun sub-mat (terms i j)
  (loop with r = 0
	 for row in terms nconc
	   (prog1
		   (unless (= r j)
			 (list 
			  (loop with c = 0
				 for x in row nconc
				   (prog1
					   (unless (= c i)
						 (list x))
					 (incf c)))))
		 (incf r))))

(defun det (mat)
  "Compute the determinant of a matrix of numbers"
  (labels ((sub-det (terms)
			 (let ((n (length terms)))
			   (cond
				 ((= n 1)
				  (car terms))
				 ((= n 2)
				  (destructuring-bind ((x11 x12) (x21 x22)) terms
					(- (* x11 x22) (* x12 x21))))
				 (t
				  (loop with s = 1
					 with row = (car terms)
					 for i below n sum
					   (let ((d (* s (car row) (sub-det (sub-mat terms i 0)))))
						 (setf s (- s)
							   row (cdr row))
						 d)))))))
	(sub-det (if (arrayp mat)
				 (mat-list mat)
				 mat))))

(defun fpoly-det (mat)
  "Compute the determinant of a matrix of polys"
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




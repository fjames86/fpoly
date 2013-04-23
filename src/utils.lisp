

;;;;
;;;; A set of utilities that form part of the fpoly package
;;;; Frank James 2013
;;;;

(in-package #:fpoly)

;; ----------------------------------------

;; factorials using cached values up to 500 (should easily be big enough)
(let* ((max-fac 500)
	   (table (make-array max-fac)))
  (labels ((gen-fac (n)
			 (labels ((rec (n acc)
						(if (= n 0)
							acc
							(rec (1- n) (* acc n)))))
			   (rec n 1))))
	(dotimes (i max-fac)
	  (setf (svref table i) (gen-fac i)))
 	
	(defun factorial (n)
	  "Factorial using cached values"
	  (if (< n max-fac)
		  (svref table n)
		  (gen-fac n)))))

(defun ncr (n k)
  "nCr combination function"
  (/ (factorial n)
     (* (factorial k) (factorial (- n k)))))

;; drop from the front of a list until a predicate returns true
(defun drop-until (test list)
  "Drop from a list until a predicate is true"
  (labels ((rec (sub-list)
			 (cond
			   ((null sub-list) nil)
			   ((funcall test (car sub-list))
				sub-list)
			   (t (rec (cdr sub-list))))))
	(rec list)))

(defun remove-nth (list n)
  (labels ((rec (list i acc)
			 (cond
			   ((null list) acc)
			   ((= i n)
				(append acc (cdr list)))
			   (t
				 (rec (cdr list)
					  (1+ i)
					  (append acc (list (car list))))))))
	(rec list 0 nil)))

;; copy a 2d array (matrix)
(defun copy-array (array &key
				   (element-type (array-element-type array))
				   (fill-pointer (and (array-has-fill-pointer-p array)
									  (fill-pointer array)))
				   (adjustable (adjustable-array-p array)))
  "Returns an undisplaced copy of ARRAY, with same fill-pointer and
adjustability (if any) as the original, unless overridden by the keyword
arguments."
  (let* ((dimensions (array-dimensions array))
		 (new-array (make-array dimensions
								:element-type element-type
								:adjustable adjustable
								:fill-pointer fill-pointer)))
	(dotimes (i (array-total-size array))
	  (setf (row-major-aref new-array i)
			(row-major-aref array i)))
	new-array))

;;; ---------------- some useful number routines -----------

;; several prime number related functions follow
(defun primes (n)
  "Find all the primes from 3 up to n. Uses Eratosthenes' sieve to find them"
  (do ((primes nil)
	   (nums (loop for i from 3 to n by 2 collect i)))
	  ((null nums) (nreverse primes))
	(let ((num (pop nums)))
	  (push num primes)
	  (setf nums (remove-if (lambda (m)
							  (zerop (mod m num)))
							nums)))))

(let* ((prime-list (cons 2 (primes 1000)))
	   (max-n (reduce #'* prime-list)))
  (defun prime-factors (n)
	"Get a list of prime factors for the number"
	(labels ((rec (pms factors n)
			   (cond
				 ((= n 1) factors)
				 ((or (null pms)
					  (> (car pms) n))
				  (cons n factors))
				 (t (let ((p (car pms)))
					  (multiple-value-bind (q r) (truncate n p)
						(if (zerop r)
							(rec pms
								 (cons p factors)
								 q)
							(rec (cdr pms)
								 factors
								 n))))))))
	  (if (<= n max-n)
		  (rec prime-list nil n)
		  (error "Trying to split ~A, largest number supported is ~A~%"
				 n max-n)))))

(defun prime? (n)
  "Predicate for a prime number"
  (let ((factors (prime-factors n)))
	(and (= (length factors) 1)
		 (= (car factors) n))))
 
(defun duplicates-p (list)
  "Returns true if the list contains duplicates"
  (labels ((rec (l1 l2)
			 (cond
			   ((and (null l1) (null l2))
				nil)
			   ((or (and (null l1) l2)
					(and l1 (null l2)))
				t)
			   (t (rec (cdr l1) (cdr l2))))))
	(rec list (remove-duplicates list))))

(defun coprime? (n1 n2)
  "Returns true if two numbers do not share any prime factors"
  (let ((facs1 (prime-factors n1))
		(facs2 (prime-factors n2)))
	(not (duplicates-p (append facs1 facs2)))))

;; ---------------------- some number theory related functions --------------

(defun egcd (a b)
  "Extended Greatest Comment Denominator"
  (if (zerop b)
	  (values 1 0)
	  (multiple-value-bind (q r) (truncate a b)
		(multiple-value-bind (x y) (egcd b r)
		  (values y (- x (* q y)))))))

;; need to check that all ni are coprime
(defun chinese-remainder (alist nlist)
  "Chinese remainder given the set of congruences x = a_i (mod n_i). Solve these for x"
  (let ((n (reduce #'* nlist)))
	(labels ((rec (alist nlist acc)
			   (if (or (null alist) (null nlist))
				   acc
				   (let* ((ai (car alist))
						  (ni (car nlist))
						  (np (/ n ni)))
					 (multiple-value-bind (p qi) (egcd ni np)
					   (declare (ignore p))
					   (rec (cdr alist)
							(cdr nlist)
							(+ acc (* ai qi np))))))))
	  (fpoly-mod (rec alist nlist 0) n))))

;;; ------- various utilities for manipulating powers and vars -----------

(defun test-list (test source-list result-list)
  "Utility to find matching value in paired lists"
  (labels ((rec (source-list result-list)
			 (if (or (null source-list) (null result-list))
				 (values nil nil)
				 (let ((result (funcall test (car source-list))))
				   (if result
					   (values (car result-list) t)
					   (rec (cdr source-list) (cdr result-list)))))))
	(rec source-list result-list)))


(defun merge-powers (vars1 powers1 vars2 powers2)
  "add all the powers together"
  (labels ((rec (bindings1 bindings2 acc)
			 (cond
			   ((null bindings1)
				(append acc bindings2))
			   ((null bindings2)
				(append acc bindings1))
			   (t
				(let* ((var (caar bindings1))
					   (val (cdar bindings1))
					   (pair (assoc var bindings2)))
				  (if pair
					  (rec (cdr bindings1) 
						   (remove-if (lambda (b)
										(eq (car b) var))
									  bindings2)
						   (append acc
								   (list (cons var (+ val (cdr pair))))))
					  (rec (cdr bindings1)
						   bindings2
						   (append acc (list (car bindings1))))))))))
    (mapcar #'cdr
			(rec (mapcar #'cons vars1 powers1)
				 (mapcar #'cons vars2 powers2)
				 nil))))

(defun merge-powers-onto (target-vars vars1 powers1 vars2 powers2)
  (mapcar (lambda (var)
			(+ (multiple-value-bind (p1 found1) (test-list (lambda (x) (eq x var))
														   vars1 powers1)
				 (if found1 p1 0))
			   (multiple-value-bind (p2 found2) (test-list (lambda (x) (eq x var))
														   vars2 powers2)
				 (if found2 p2 0))))
		  target-vars))


(defun merge-vars (vars1 vars2)
  (labels ((rec (vars1 vars2 acc)
			 (cond
			   ((null vars1)
				(append acc vars2))
			   ((null vars2)
				(append acc vars1))
			   (t
				(let* ((var (car vars1))
					   (pair (find var vars2)))
				  (if pair
					  (rec (cdr vars1)
						   (remove-if (lambda (b)
										(eq b var))
									  vars2)
						   (append acc (list var)))
					  (rec (cdr vars1)
						   vars2
						   (append acc (list var)))))))))
    (rec vars1 vars2 nil)))

(defun project-powers (source-vars source-powers target-vars)
  "Project the bindings down onto the vars, which must form a sub-basis of the bindings"
  (labels ((rec (source-vars source-powers acc)
			 (if (or (null source-vars) (null source-powers))
				 acc
				 (let ((var (find (car source-vars) target-vars))
					   (power (car source-powers)))
				   (cond
					 (var
					  (rec (cdr source-vars)
						   (cdr source-powers)
						   (cons power acc)))
					 ((zerop power)
					  (rec (cdr source-vars)
						   (cdr source-powers)
						   acc))
					 (t nil))))))
	(nreverse (rec source-vars source-powers nil))))

(defun shuffle-power-order (source-vars source-powers target-vars)
  "Switch power order around, e.g. from (x=1, y=2) -> (y=2, x=1)"
  (mapcar (lambda (var)
			(multiple-value-bind (power found) (test-list (lambda (x) (eq x var))
														 source-vars source-powers)
			  (if found
				  power
				  (error "*** shuffle-power-order: ~A not found in source vars" var))))
		  target-vars))


		 



(in-package #:fpoly)

;;; ------------ utils --------------------

(defun factorial (n)
  (labels ((rec (n acc)
			 (if (= n 0)
				 acc
				 (rec (1- n) (* acc n)))))
    (rec n 1)))

(defun ncr (n k)
  (/ (factorial n)
     (* (factorial k) (factorial (- n k)))))

;;; ------------- offset calculations -------

(defun base-offset (num-vars degree)
  (if (< degree 0)
      0
      (ncr (+ num-vars degree)
		   degree)))

(defun number-terms (num-vars degree)
  (if (= degree 0)
      1
      (- (base-offset num-vars degree)
		 (base-offset num-vars (1- degree)))))

(defun power-offset (powers)
  (let ((degree (reduce #'+ powers))
		(power (car powers)))
    (cond
      ((or (null (cdr powers))
		   (= degree 0))
       0)
      ((= power 0)
       (+ (number-terms (length powers) (1- degree))
		  (power-offset (cdr powers))))
      (t
       (power-offset (cons (1- power)
						   (cdr powers)))))))

(defun offset (powers)
  (+ (base-offset (length powers) (1- (reduce #'+ powers)))
     (power-offset powers)))


(defun gen-powers (n-vars degree)
  (cond
    ((zerop n-vars)
     nil)
    ((= degree 0)
     (list (make-list n-vars :initial-element 0)))
    ((= n-vars 1)
     (list (list degree)))
    (t (do ((n 0 (1+ n))
			(terms nil))
		   ((> n degree) (nreverse terms))
		 (mapc (lambda (p)
				 (push (append p (list n)) terms))
			   (gen-powers (1- n-vars) (- degree n)))))))

(defun gen-all-powers (n-vars degree)
  (loop for n below (1+ degree) nconc (gen-powers n-vars n)))

;;;; ----------- class definitions -----------------

;; macro to iterate over coefficients in a flat poly
(defmacro docoeffs ((fpoly coeff-var powers-var &optional index-var) &body body)
  (let ((gpowers (gensym "POWERS"))
		(gc (gensym "COEFFS"))
		(gi (if index-var index-var (gensym "INDEX")))
		(gp (gensym "POLY")))	 
    `(let* ((,gp ,fpoly)
			(,gc (fpoly-coeffs ,gp)))
	   (declare (ignorable ,gc))
       (do ((,gi 0 (1+ ,gi))
			(,gpowers (fpoly-powers ,gp) (cdr ,gpowers)))
		   ((null ,gpowers))
		 (let ((,powers-var (car ,gpowers)))
		   (declare (ignorable ,powers-var))
		   (symbol-macrolet ((,coeff-var (svref ,gc ,gi)))
			 ,@body))))))

(defclass fpoly ()
  ((vars :reader fpoly-vars :initarg :vars)
   (degree :reader fpoly-degree :initarg :degree :initform 0)
   (size :accessor fpoly-size :initarg :size)
   (coeffs :reader fpoly-coeffs :writer set-fpoly-coeffs :initarg :coeffs)
   (powers :reader fpoly-powers :initarg :powers)))

(defun make-fpoly (vars degree &optional coeffs)  
  "Make an fpoly object"
  (let ((size (base-offset (length vars) degree)))
    (make-instance 'fpoly 
				   :vars vars
				   :degree degree
				   :size size
				   :powers (gen-all-powers (length vars) degree)
				   :coeffs (cond
							 ((null coeffs) 
							  (make-array size :initial-element 0))
							 ((arrayp coeffs)
							  coeffs)			     
							 ((listp coeffs)
							  (make-array size :initial-contents coeffs))
							 (t (make-array size :initial-element 0))))))

(defun fpoly? (p) 
  "Predicate for fpoly"
  (typep p 'fpoly))

;; getter
(defun fpoly-coeff (p &rest powers)
  "Get the coefficient with the powers specified. 
Returns zero if this is outside the array"
  (let ((o (offset powers)))
    (if (< o (fpoly-size p))	
		(svref (fpoly-coeffs p) o)
		0)))

;; setter for internal use only i.e. not exported
(defun (setf fpoly-coeff) (val p &rest powers)
  "Set the coefficient with the powers specified"
  (let ((o (offset powers))
		(c (fpoly-coeffs p)))        
    (cond      
      ((< o (fpoly-size p))
       (setf (svref c o) val)
       (set-fpoly-coeffs c p))
      (t (error "*** setf fpoly-size: offset lies outside of coeff array")))
    p))

(defun print-fpoly (p &optional (stream *standard-output*))
  "Standard polynomial printing, i.e. 1 + X^2 + ..."
  (let ((vars (fpoly-vars p))
		(printed nil))
    (docoeffs (p coeff powers index)
			  (cond
				((zerop coeff) nil)
				(t
				 (if printed
					 (format t " + "))
				 (if (> index 0)
					 (unless (= coeff 1)
					   (format stream "~A*" coeff))
					 (format stream "~A" coeff))
				 (mapc (lambda (x n)
						 (cond
						   ((= n 0) nil)
						   ((= n 1)
							(format stream "~A" x))
						   (t (format stream "~A^~A" x n))))
					   vars powers)
				 (setf printed t))))
	(if (not printed)
		(print 0))))

;; generic method for lisp printer. will print "nicely" for princ or format ~A 
(defmethod print-object ((p fpoly) stream)
  (if *print-escape*
      (print-unreadable-object (p stream :type t)
		(format stream ":VARS ~A :DEGREE ~A :COEFFS ~A"
				(fpoly-vars p)
				(fpoly-degree p)
				(fpoly-coeffs p)))
      (print-fpoly p stream)))

;;; -----------------------

(defun test-list (test source-list result-list)
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

;;;; generic functions

(defgeneric fpoly-add (p1 p2)
  (:documentation "Generic addition of polynomials"))

(defgeneric fpoly-sub (p1 p2)
  (:documentation "Generic subtraction of polynomials"))

(defgeneric fpoly-mul (p1 p2)
  (:documentation "Generic multiplication of polynomials"))

;;; --------------------


(defmethod fpoly-add (p1 p2)
  (+ p1 p2))

(defmethod fpoly-add ((p1 fpoly) (p2 number))
  (let ((c (copy-seq (fpoly-coeffs p1))))
	(incf (svref c 0) p2)
	(make-fpoly (fpoly-vars p1)
				(fpoly-degree p1)
				c)))

(defmethod fpoly-add ((p1 number) (p2 fpoly))
  (let ((c (copy-seq (fpoly-coeffs p2))))
	(incf (svref c 0) p1)
	(make-fpoly (fpoly-vars p2)
				(fpoly-degree p2)
				c)))

(defmethod fpoly-add ((p1 fpoly) (p2 fpoly))
  (let* ((degree (max (fpoly-degree p1) (fpoly-degree p2)))
		 (vars (merge-vars (fpoly-vars p1) (fpoly-vars p2)))
		 (p (make-fpoly vars degree)))
	(docoeffs (p coeff powers index)
	  (let ((p1-powers (project-powers vars powers (fpoly-vars p1)))
			(p2-powers (project-powers vars powers (fpoly-vars p2))))
		   (let ((c1 (if p1-powers (apply #'fpoly-coeff p1 p1-powers) 0))
				 (c2 (if p2-powers (apply #'fpoly-coeff p2 p2-powers) 0)))
			 (setf coeff (+ c1 c2)))))
	p))

;; ------------------


(defmethod fpoly-sub ((p1 number) (p2 number))
  (- p1 p2))

(defmethod fpoly-sub ((p1 fpoly) (p2 number))
  (let ((c (copy-seq (fpoly-coeffs p1))))
	(decf (svref c 0) p2)
	(make-fpoly (fpoly-vars p1)
				(fpoly-degree p1)
				c)))

(defmethod fpoly-sub ((p1 number) (p2 fpoly))
  (let ((c (map 'vector #'- (fpoly-coeffs p2))))
	(incf (svref c 0) p1)
	(make-fpoly (fpoly-vars p2)
				(fpoly-degree p2)
				c)))

(defmethod fpoly-sub ((p1 fpoly) (p2 fpoly))
  (let* ((degree (max (fpoly-degree p1) (fpoly-degree p2)))
		 (vars (merge-vars (fpoly-vars p1) (fpoly-vars p2)))
		 (p (make-fpoly vars degree)))
	(docoeffs (p coeff powers index)
		 (let ((p1-powers (project-powers vars powers (fpoly-vars p1)))
			   (p2-powers (project-powers vars powers (fpoly-vars p2))))
		   (let ((c1 (if p1-powers (apply #'fpoly-coeff p1 p1-powers) 0))
				 (c2 (if p2-powers (apply #'fpoly-coeff p2 p2-powers) 0)))
			 (setf coeff (- c1 c2)))))
	p))

;; -----------

(defmethod fpoly-mul ((p1 number) (p2 number))
  (* p1 p2))

(defmethod fpoly-mul ((p1 fpoly) (p2 number))
  (make-fpoly (fpoly-vars p1)
			  (fpoly-degree p1)
			  (map 'vector
					  (lambda (x)
						(* x p2))
					  (fpoly-coeffs p1))))

(defmethod fpoly-mul ((p1 number) (p2 fpoly))
  (make-fpoly (fpoly-vars p2)
			  (fpoly-degree p2)
			  (map 'vector
				   (lambda (x)
					 (* x p1))
				   (fpoly-coeffs p2))))

(defmethod fpoly-mul ((p1 fpoly) (p2 fpoly))
  (let* ((degree (+ (fpoly-degree p1) (fpoly-degree p2)))
		 (vars (merge-vars (fpoly-vars p1) (fpoly-vars p2)))
		 (p (make-fpoly vars degree)))
	(docoeffs (p1 coeff1 powers1 index1)
	  (docoeffs (p2 coeff2 powers2 index2)
		(incf (apply #'fpoly-coeff
					 p
					 (merge-powers (fpoly-vars p1) powers1
								   (fpoly-vars p2) powers2))
			  (* coeff1 coeff2))))
	p))

;;; ---------------




				


		 

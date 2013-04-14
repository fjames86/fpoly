
(in-package #:fpoly)

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

;; class definition
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

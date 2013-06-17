
;;;;
;;;; Defines the base class for polynomials and associated routines for
;;;; accessing data slots, printing, parsing etc.
;;;;

(in-package #:fpoly)


(defparameter *fpoly-debug* t)

(defun fpoly-debug (control-string &rest format-args)
  (if *fpoly-debug*
	  (apply #'format *fpoly-debug* control-string format-args)))

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

(defun number-terms% (num-vars degree)
  (cond
	((<= num-vars 1)
	 1)
	((zerop degree)
	 1)
	((= degree 1)
	 num-vars)
	(t 
	 (+ (number-terms% num-vars (1- degree))
		(number-terms% (1- num-vars) degree)))))

(defun power-offset (powers)
  (let ((degree (reduce #'+ (cdr powers))))
	(cond
	  ((or (null (cdr powers))
		   (= (+ (car powers) degree) 0))
	   0)
	  (t 
	   (+ (number-terms (length powers) (1- degree))
		  (power-offset (cdr powers)))))))

(defun offset (powers)
  (+ (base-offset (length powers) (1- (reduce #'+ powers)))
	 (power-offset powers)))

;; precomputed powers list

(let* ((max-nvars 5)
	   (max-degree 20)
	   (powers-table (make-array (list max-nvars (1+ max-degree)) :initial-element nil)))
  (labels ((gen-power-list (n-vars degree)
			 (cond
			   ((zerop n-vars)
				nil)
			   ((zerop degree)
				(list (make-list n-vars :initial-element 0)))
			   ((= n-vars 1)
				(list (list degree)))
			   (t (do ((n degree (1- n))
					   (terms nil))
					  ((< n 0) (nreverse terms))
					(mapc (lambda (p)
							(push (cons n p) terms))
						  (gen-power-list (1- n-vars) (- degree n))))))))
	(loop for nvars from 1 to max-nvars do
		 (loop for degree from 0 to max-degree do
			  (setf (aref powers-table (1- nvars) degree)
					(gen-power-list nvars degree))))

	(defun gen-powers (nvars degree)
	  (cond
		((<= nvars 0) nil)
	   ((and (<= nvars max-nvars)
			 (<= degree max-degree))
		(aref powers-table (1- nvars) degree))
	   (t (gen-power-list nvars degree))))))

(defun gen-all-powers (n-vars degree)
  "Generate a list of all power coordinates"
  (loop for n from 0 to degree append (gen-powers n-vars n)))


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

(defmacro docoeffs* ((fpoly coeff-var powers-var &optional index-var) &body body)
  (let ((gpowers (gensym "POWERS"))
		(gc (gensym "COEFFS"))
		(gi (if index-var index-var (gensym "INDEX")))
		(gp (gensym "POLY")))	 
    `(let* ((,gp ,fpoly)
			(,gc (fpoly-coeffs ,gp)))
	   (declare (ignorable ,gc))
       (do ((,gi (1- (fpoly-size ,gp)) (1- ,gi))
			(,gpowers (reverse (fpoly-powers ,gp)) (cdr ,gpowers)))
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

(defmethod fpoly-degree ((p number))
  0)

(defmethod fpoly-vars ((p number))
  nil)

(defmethod fpoly-coeffs ((p number))
  (make-array 1 :initial-contents (list p)))

(defun make-fpoly (variables degree &optional coeffs)  
  "Make an fpoly object."
  (let* ((vars (cond
				 ((null variables) nil)
				 ((symbolp variables)
				  (list variables))
				 (t variables)))
		 (size (base-offset (length vars) degree)))
	(make-instance 'fpoly 
				   :vars vars
				   :degree (if (= size 1) 0 degree)
				   :size size
				   :powers (if (= size 1)
							   (gen-all-powers 1 0)
							   (gen-all-powers (length vars) degree))
				   :coeffs (cond
							 ((null coeffs) 
							  (make-array size :initial-element 0))
							 ((arrayp coeffs)
							  coeffs)			     
							 ((listp coeffs)
							  (make-array size :initial-contents coeffs))
							 (t (make-array size :initial-element 0))))))

(defun make-monomial (variables powers &optional (coeff 1))
  "Make a single variable polynomial with a single non-zero coefficient at degree n"
  (let ((p (make-fpoly variables (reduce #'+ powers))))
	(setf (apply #'fpoly-coeff p powers) coeff)
	p))

(defun fpoly? (p) 
  "Predicate for fpoly"
  (typep p 'fpoly))

;; getter
(defun fpoly-coeff (p &rest powers)
  "Get the coefficient with the powers specified. 
Returns zero if this is outside the array"
  (let ((o (if (cdr powers) (offset powers) (car powers))))
    (if (< o (fpoly-size p))	
		(svref (fpoly-coeffs p) o)
		0)))

;; setter 
(defun (setf fpoly-coeff) (val p &rest powers)
  "Set the coefficient with the powers specified"
  (let ((o (offset powers))
		(c (fpoly-coeffs p)))        
    (cond
      ((< o (fpoly-size p))
       (setf (svref c o) val)
       (set-fpoly-coeffs c p))
      (t (error 'fpoly-error
				:place "FPOLY-SIZE"
				:data "Offset lies outside of coeff array")))
    p))

(defun print-coeff (c stream)
  (cond
	((complexp c)
	 (format stream "(~A " (realpart c))
	 (if (< (imagpart c) 0)
		 (princ " - " stream)
		 (princ " + " stream))
	 (format stream "~A*i)" (abs (imagpart c))))
	((numberp c) (princ c stream))
	((fpoly? c)
	 (princ "(" stream)
	 (print-fpoly c stream)
	 (princ ")"))
	(t (princ c stream))))

(defun print-fpoly (p &optional (stream *standard-output*))
  "Standard polynomial printing, i.e. 1 + X^2 + ..."
  (let ((vars (fpoly-vars p))
		(printed nil))
    (docoeffs (p coeff powers index)
	  (cond
		((zerop coeff) nil)
		(t
		 (let ((sign (if (and (realp coeff) (< coeff 0)) -1 1)))
		   ;; print the +/- sign but only if adding a new term 
		   (if printed
			   (if (= sign -1)
				   (format stream " - ")
				   (format stream " + ")))
		   ;; print the coefficient number unless it's either the first term or 1
		   (cond
			 ((= index 0)
			  (print-coeff coeff stream))
			 ((= coeff -1)
			  (unless printed
				(format stream "-")))
			 ((= coeff 1)
			  nil)
			 (t (print-coeff (if (numberp coeff)
								 (* sign coeff)
								 coeff)
							 stream)
				(princ "*" stream)))
		   ;; print the variables
		   (mapc (lambda (x n)
				   (cond
					 ((= n 0) nil)
					 ((= n 1)
					  (format stream "~A" x))
					 (t (format stream "~A^~A" x n))))
				 vars powers)
		   (setf printed t)))))
	(if (not printed)
		(princ 0 stream))))

;; generic method for lisp printer. will print "nicely" for princ or format ~A 
(defmethod print-object ((p fpoly) stream)
  (if *print-escape*
	  (progn
		(princ "#{" stream)
		(print-fpoly p stream)
		(princ "}" stream))
      (print-fpoly p stream)))

(defun fpoly-info (poly &optional (stream t))
  (if (fpoly? poly)
	  (format stream "#<FPOLY :VARS ~A :DEGREE ~A :COEFFS ~A>"
			  (fpoly-vars poly) (fpoly-degree poly) (fpoly-coeffs poly))
	  (format stream "~A" poly)))

;;; -------------

(defun fpoly-chinese-remainder (polys nlist)
  "Chinese remainder each coefficient of the polynomials provided."
  (let ((vars (remove-duplicates (mapcan (lambda (poly)
										   (if (fpoly? poly)
											   (mapcar #'identity (fpoly-vars poly))))
										 polys)))
		(degree (apply #'max (mapcar (lambda (poly)
									   (if (fpoly? poly)
										   (fpoly-degree poly)
										   0))
									 polys))))
	(let ((p (make-fpoly vars degree)))
	  (docoeffs (p coeff powers)
		(setf coeff (chinese-remainder (mapcar (lambda (poly)
												 (cond
												   ((fpoly? poly)
													(apply #'fpoly-coeff poly powers))
												   ((zerop (reduce #'+ powers))
													poly)
												   (t 0)))
											   polys)
									   nlist)))
	  p)))


(defun fpoly-density (poly)
  "Find the density of coefficients"
  (if (fpoly? poly)
	  (let ((coeffs (fpoly-coeffs poly)))
		(/(loop for i below (fpoly-size poly) sum
			   (if (zerop (svref coeffs i))
				   0
				   1))
		  (fpoly-size poly)))
	  0))

(defun fpoly-shuffle (poly ordering)
  "Reorder a polynomials variables"
  (let ((p (make-fpoly ordering (fpoly-degree poly)))
		(vars (fpoly-vars poly)))
	(docoeffs (poly coeff powers)
	  (setf (apply #'fpoly-coeff p (project-powers-onto vars powers ordering))
			coeff))
	p))


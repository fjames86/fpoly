
;;;;
;;;; Defines all operations that are performed on polynomials (and numbers)
;;;; as generic functions. Essentially promotes polynomials as a superclass of numbers
;;;; 

(in-package #:fpoly)


;;;; generic functions ------------------------------

(defgeneric fpoly-add (p1 p2)
  (:documentation "Generic addition of polynomials"))

(defgeneric fpoly-sub (p1 p2)
  (:documentation "Generic subtraction of polynomials"))

(defgeneric fpoly-mul (p1 p2)
  (:documentation "Generic multiplication of polynomials"))

(defgeneric fpoly-div (p1 p2)
  (:documentation "Generic division of polynomials"))

(defgeneric fpoly-eql (p1 p2)
  (:documentation "Generic equality of polynomials"))

(defgeneric fpoly-mod (poly divisor)
  (:documentation "Generic modulo"))

(defgeneric fpoly-expt (poly n)
  (:documentation "Generic expt function"))

(defgeneric fpoly-eval (poly bindings)
  (:documentation "Evaluation of a polynomial"))

(defgeneric fpoly-copy (poly)
  (:documentation "Copy a polynomial or number"))

(defgeneric fpoly-substitute (poly var val)
  (:documentation "Form a new polynomial with the variable substituted for a value"))

(defgeneric fpoly-simplify (poly)
  (:documentation "Reduce the degree if higher coefficients are zero"))

;;; -------------------- addition ---------------------


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

;; useful macro, same as incf but uses fpoly-add instead of +
(defmacro fpoly-incf (place &optional (amount 1))
  (let ((gnew (gensym "NEW"))
		(gamount (gensym "AMOUNT")))
	`(let* ((,gamount ,amount)
			(,gnew (fpoly-add ,place ,gamount)))
	   (setf ,place ,gnew))))

;; ------------------ subtraction ----------------


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

;; ----------- multiplication ----------------

(defmethod fpoly-mul ((p1 number) (p2 number))
  (* p1 p2))

(defmethod fpoly-mul ((p1 fpoly) (p2 number))
  (cond
	((zerop p2) 0)
	((= p2 1) (fpoly-copy p1))
	(t
	  (make-fpoly (fpoly-vars p1)
				  (fpoly-degree p1)
				  (map 'vector
					   (lambda (x)
						 (* x p2))
					   (fpoly-coeffs p1))))))

(defmethod fpoly-mul ((p1 number) (p2 fpoly))
  (cond
	((zerop p1) 0)
	((= p1 1) (fpoly-copy p2))
	(t
	 (make-fpoly (fpoly-vars p2)
				 (fpoly-degree p2)
				 (map 'vector
					  (lambda (x)
						(* x p1))
					  (fpoly-coeffs p2))))))

(defmethod fpoly-mul ((p1 fpoly) (p2 fpoly))
  (let* ((degree (+ (fpoly-degree p1) (fpoly-degree p2)))
		 (vars (merge-vars (fpoly-vars p1) (fpoly-vars p2)))
		 (p (make-fpoly vars degree)))
	(docoeffs (p1 coeff1 powers1 index1)
	  (docoeffs (p2 coeff2 powers2 index2)
		(incf (apply #'fpoly-coeff
					 p
					 (merge-powers-onto vars
										(fpoly-vars p1) powers1
										(fpoly-vars p2) powers2))
			  (* coeff1 coeff2))))
	p))

;; ------------ division -----------

(defmethod fpoly-div ((p1 number) (p2 number))
  (/ p1 p2))

(defmethod fpoly-div ((p1 fpoly) (p2 number))
  (fpoly-mul p1 (/ p2)))

(defmethod fpoly-div ((p1 number) (p2 fpoly))
  (fpoly-mul p2 (/ p1)))

(defmethod fpoly-div ((p1 fpoly) (p2 fpoly))
  (error "*** fpoly-div: poly division not yet implemented"))


;;; --------------- equality testing -------------

(defmethod fpoly-eql ((p1 number) (p2 number))
  (= p1 p2))

(defmethod fpoly-eql ((p1 fpoly) (p2 number))
  (and (= (svref (fpoly-coeffs p1) 0) p2)
	   (every (lambda (n)
				(= n 0))
			  (subseq (fpoly-coeffs p1) 1))))

(defmethod fpoly-eql ((p1 number) (p2 fpoly))
  (fpoly-eql p2 p1))

(defmethod fpoly-eql ((p1 fpoly) (p2 fpoly))
  (if (and (= (fpoly-degree p1) (fpoly-degree p2))
		   (not (set-exclusive-or (fpoly-vars p1) (fpoly-vars p2))))
	  (let ((e t))
		(docoeffs (p1 c1 powers1)
		  (unless (= c1 (apply #'fpoly-coeff
							   p2
							   (shuffle-power-order (fpoly-vars p1)
													powers1
													(fpoly-vars p2))))
			(setf e nil)))
		e)))

;; -------------- modulo --------------

(defmethod fpoly-mod ((poly number) (divisor integer))
  "Polynomial modulo of numbers.
Always choose the (absolute value) which is smaller of the two options"
  (let ((x (mod poly divisor)))
	(if (< (abs x) (abs (- x divisor)))
		x
		(- x divisor))))

(defmethod fpoly-mod ((poly fpoly) (divisor integer))
  (make-fpoly (fpoly-vars poly)
			  (fpoly-degree poly)
			  (map 'vector
				   (lambda (n)
					 (fpoly-mod n divisor))
				   (fpoly-coeffs poly))))

(defun mod-add (x y prime)
  (fpoly-mod (+ (fpoly-mod x prime) (fpoly-mod y prime)) prime))

(defun mod-sub (x y prime)
  (fpoly-mod (- (fpoly-sub x prime) (fpoly-mod y prime)) prime))

(defun mod-mul (x y prime)
  (fpoly-mod (* (fpoly-mod x prime) (fpoly-mod y prime)) prime))

(defun replace-car (list replace-alist prime)
  (cond
	((null list) nil)
	((listp list)
	 (let* ((c (car list))
			(val (assoc c replace-alist)))
	   (if val
		   (append (cons (cdr val)
						 (mapcar (lambda (x)
								   (replace-car x replace-alist prime))
								 (cdr list)))
				   (list prime))
		   (cons c
				 (mapcar (lambda (x)
						   (replace-car x replace-alist prime))
						 (cdr list))))))
	(t list)))

(defmacro with-modular-arithmetic (prime &body body)
  (let ((gprime (gensym "PRIME")))
	(if prime
		`(let ((,gprime ,prime))
		   (progn
			 ,@(mapcar (lambda (exp)
						 (replace-car exp
									  '((+ . mod-add)
										(- . mod-sub)
										(* . mod-mul))
									  gprime))
					   body)))
		`(progn ,@body))))
	   

;; ------------------ expt -------------------------

(defmethod fpoly-expt ((poly number) (n integer))
  (expt poly n))

(defun fpoly-sq (p)
  (fpoly-mul p p))

(defmethod fpoly-expt ((poly fpoly) (n integer))
  (cond
	((zerop n) 1)
	((= n 1) poly)
	(t
	 (if (evenp n)
		 (fpoly-sq (fpoly-expt poly (/ n 2)))
		 (fpoly-mul (fpoly-expt poly (/ (1- n) 2))
					(fpoly-expt poly (1+ (/ (1- n) 2))))))))
   
;; ------------- eval ----------------

(defmethod fpoly-eval ((poly number) bindings)
  poly)

(defun fpoly-eval-monomial (var-vals powers &optional (coeff 1))
  "Evaluate a term coeff*X^n*Y^m... for various variables X,Y... and powers"
  (fpoly-mul coeff (reduce #'fpoly-mul
						   (mapcar (lambda (val n)
									 (cond
									   ((symbolp val)
										(make-monomial (list val) (list n)))
									   (t (fpoly-expt val n))))
								   var-vals powers))))

(defmethod fpoly-eval ((poly fpoly) bindings)
  "Evaluate a polynomial with bindings an assoc list of (var . value) pairs.
The values may be other polynomials or numbers."
  (let ((sum 0)
		(vals (mapcar #'cdr bindings)))
	(docoeffs (poly coeff powers)
	  (fpoly-incf sum (fpoly-eval-monomial vals powers coeff)))
	sum))

;; ------------- copying ------------------------

(defmethod fpoly-copy ((poly number))
  poly)

(defmethod fpoly-copy ((poly fpoly))
  (make-fpoly (fpoly-vars poly)
			  (fpoly-degree poly)
			  (copy-seq (fpoly-coeffs poly))))


;; ------------ substitute --------------------------

(defmethod fpoly-substitute ((poly fpoly) (var symbol) (val number))
  "Substitute a variable for a number"
  (let* ((vars (fpoly-vars poly))
		 (var-n (position var vars))
		 (p (make-fpoly (remove var vars) (fpoly-degree poly))))
	(docoeffs (poly coeff powers)
	  (let ((p-powers (remove-nth powers var-n))
			(var-val (expt val (nth var-n powers))))
		(incf (apply #'fpoly-coeff p p-powers) (* coeff var-val))))
	p))
	  
(defmethod fpoly-substitute ((poly fpoly) (var symbol) (val symbol))
  "Change a variable name"
  (make-fpoly (mapcar (lambda (x)
						(if (eq x var)
							val
							x))					  
					  (fpoly-vars poly))
			  (fpoly-degree poly)
			  (copy-array (fpoly-coeffs poly))))

(defmethod fpoly-substitute ((poly fpoly) (var symbol) (val fpoly))
  "Substitute a variable for a polynomial"
  (fpoly-eval poly
			  (mapcar (lambda (v)
						(if (eq v var)
							(cons v val)
							(cons v v)))
					  (fpoly-vars poly))))

;; ----------------- simplification ---------------------

(defun highest-degree (poly)
  "Find the highest degree of the non-zero coefficients"
  (let ((degree 0)
		(pws nil))
	(docoeffs (poly coeff powers)
	  (unless (zerop coeff)
		(let ((d (reduce #'+ powers)))
		  (if (> d degree)
			  (setf degree d
					pws powers)))))
	(values degree pws)))

(defun involved-vars (poly)
  "Find the variables with non-zero coefficients and non-zero powers"
  (let ((vars nil)
		(pvars (fpoly-vars poly)))
	(docoeffs (poly coeff powers)
	  (unless (zerop coeff)
		(mapc (lambda (var power)
				(if (> power 0)
					(pushnew var vars)))
			  pvars powers)))
	vars))

(defmethod fpoly-simplify ((poly fpoly))
  "Return a new polynomial with the minimal variables and degree possible."
  (let ((vars (involved-vars poly))
		(degree (highest-degree poly)))
	(let ((p (make-fpoly vars degree)))
	  (docoeffs (p coeff powers)
		(let ((pws (project-powers vars powers (fpoly-vars poly))))
		  (setf coeff (if pws (apply #'fpoly-coeff poly pws) 0))))
	  p)))

	  
;;; ------------ reducers -------------------------

;; reducer
;; used for summing a set of polynomial objects
;;

(defun fpoly-sum (polys)
  "Efficiently sum a list of polynomials.
Equivalent to (apply #'reduce #'fpoly-add poly polys) but faster and conses less."
  (let ((vars (reduce #'merge-vars (mapcar (lambda (poly)
											 (if (fpoly? poly)
												 (fpoly-vars poly)))
										   polys)))
		(degree (apply #'max (mapcar (lambda (poly)
									   (if (fpoly? poly)
										   (fpoly-degree poly)
										   0))
									 polys))))
	(let ((p (make-fpoly vars degree)))
	  (docoeffs (p coeff powers)
		(setf coeff
			  (loop for ply in polys sum
				   (let ((pws (project-powers vars powers (if (fpoly? ply)
															  (fpoly-vars ply)))))
															  
					 (if pws
						 (if (fpoly? ply)
							 (apply #'fpoly-coeff ply pws)
							 0)
						 0)))))
	  p)))

;;; ---------------------------------------


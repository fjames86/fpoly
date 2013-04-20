
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

(defgeneric fpoly-eval (poly bindings)
  (:documentation "Evaluation of a polynomial"))

(defgeneric fpoly-copy (poly)
  (:documentation "Copy a polynomial or number"))

(defgeneric fpoly-substitute (poly var val)
  (:documentation "Form a new polynomial with the variable substituted for a value"))

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

(defmethod fpoly-eql (p1 p2)
  (= p1 p2))

(defmethod fpoly-eql ((p1 fpoly) p2)
  (and (= (svref (fpoly-coeffs p1) 0) p2)
	   (every (lambda (n)
				(= n 0))
			  (subseq (fpoly-coeffs p1) 1))))

(defmethod fpoly-eql (p1 (p2 fpoly))
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

;; ------------- eval ----------------

(defmethod fpoly-eval ((poly number) bindings)
  poly)

(defun fpoly-eval-monomial (var-vals powers &optional (coeff 1))
  (* coeff (reduce #'* (mapcar (lambda (val n)
								 (expt val n))
							   var-vals powers))))

(defmethod fpoly-eval ((poly fpoly) bindings)
  (let ((sum 0)
		(vars (fpoly-vars poly))
		(vals (mapcar #'cdr bindings)))
	(docoeffs (poly coeff powers)
	  (incf sum (fpoly-eval-monomial vals powers coeff)))
	sum))

;; -------------

(defmethod fpoly-copy ((poly number))
  poly)

(defmethod fpoly-copy ((poly fpoly))
  (make-fpoly (fpoly-vars poly)
			  (fpoly-degree poly)
			  (copy-seq (fpoly-coeffs poly))))


;; ----------------------

(defmethod fpoly-substitute ((poly fpoly) (var symbol) (val number))
  (let ((p (make-fpoly (remove var (fpoly-vars poly))
					   (fpoly-degree poly))))
	p))

(defmethod fpoly-substitute ((poly fpoly) (var symbol) (val fpoly))
  (let ((p (make-fpoly (remove var (fpoly-vars poly))
					   (fpoly-degree poly))))
	p))


	  
				  

(in-package #:fpoly)


;;;; generic functions ------------------------------

(defgeneric fpoly-add (p1 p2)
  (:documentation "Generic addition of polynomials"))

(defgeneric fpoly-sub (p1 p2)
  (:documentation "Generic subtraction of polynomials"))

(defgeneric fpoly-mul (p1 p2)
  (:documentation "Generic multiplication of polynomials"))

(defgeneric fpoly-eql (p1 p2)
  (:documentation "Generic equality of polynomials"))

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
					 (merge-powers (fpoly-vars p1) powers1
								   (fpoly-vars p2) powers2))
			  (* coeff1 coeff2))))
	p))

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

;;;; --------------


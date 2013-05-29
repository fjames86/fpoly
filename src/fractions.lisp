
(in-package #:fpoly)

(defclass fpoly-fraction ()
    ((dividend :reader frac-dividend :initarg :dividend)
	    (divisor :reader frac-divisor :initarg :divisor)
	    (q :reader frac-q :initarg :q)
	    (r :reader frac-r :initarg :r)))

(defun make-frac (dividend divisor)
    (multiple-value-bind (q r) (fpoly-div dividend divisor)
	      (if (fpoly-zerop r)
			  q
			  (make-instance 'fpoly-fraction
							   :dividend dividend
							     :divisor divisor
								   :q q
								     :r r))))

(defun frac? (frac)
  (typep frac 'fpoly-fraction))

(defmethod print-object ((frac fpoly-fraction) stream)
    (if *print-escape*
		      (princ "#{" stream))
	  (format stream "(~A) / (~A)" (frac-dividend frac) (frac-divisor frac))
	    (if *print-escape*
			      (princ "}" stream)))

;; ---

(defmethod fpoly-add ((p1 fpoly-fraction) (p2 fpoly-fraction))
    (make-frac (fpoly-add (fpoly-mul (frac-dividend p1) (frac-divisor p2))
						  (fpoly-mul (frac-dividend p2) (frac-divisor p1)))
			       (fpoly-mul (frac-divisor p1) (frac-divisor p2))))

(defmethod fpoly-add ((p1 fpoly-fraction) p2)
    (make-frac (fpoly-add (frac-dividend p1)
						  (fpoly-mul p2 (frac-divisor p1)))
			       (frac-divisor p1)))

(defmethod fpoly-add (p1 (p2 fpoly-fraction))
    (make-frac (fpoly-add (frac-dividend p2)
						  (fpoly-mul p1 (frac-divisor p2)))
			       (frac-divisor p2)))

(defmethod fpoly-sub ((p1 fpoly-fraction) (p2 fpoly-fraction))
    (make-frac (fpoly-sub (fpoly-mul (frac-dividend p1) (frac-divisor p2))
						  (fpoly-mul (frac-dividend p2) (frac-divisor p1)))
			       (fpoly-mul (frac-divisor p1) (frac-divisor p2))))

(defmethod fpoly-sub ((p1 fpoly-fraction) p2)
    (make-frac (fpoly-sub (frac-dividend p1)
						  (fpoly-mul p2 (frac-divisor p1)))
			       (frac-divisor p1)))

(defmethod fpoly-sub (p1 (p2 fpoly-fraction))
    (make-frac (fpoly-sub (fpoly-mul p1 (frac-divisor p2))
						  (frac-dividend p2))
			       (frac-divisor p2)))


(defmethod fpoly-mul ((p1 fpoly-fraction) (p2 fpoly-fraction))
    (make-frac (fpoly-mul (frac-dividend p1) (frac-dividend p2))
			       (fpoly-mul (frac-divisor p1) (frac-divisor p2))))

(defmethod fpoly-mul ((p1 fpoly-fraction) p2)
    (make-frac (fpoly-mul (frac-dividend p1) p2)
			       (frac-divisor p1)))

(defmethod fpoly-mul (p1 (p2 fpoly-fraction))
    (make-frac (fpoly-mul (frac-dividend p2) p1)
			       (frac-divisor p2)))

(defmethod fpoly-div ((p1 fpoly-fraction) (p2 fpoly-fraction))
    (values (make-frac (fpoly-mul (frac-dividend p1) (frac-divisor p2))
					       (fpoly-mul (frac-divisor p1) (frac-dividend p2)))
			 0))

(defmethod fpoly-div ((p1 fpoly-fraction) p2)
    (values (make-frac (frac-dividend p1)
					       (fpoly-mul (frac-divisor p1) p2))
			 0))

(defmethod fpoly-div (p1 (p2 fpoly-fraction))
    (values (make-frac (fpoly-mul (frac-divisor p2) p1)
					       (frac-dividend p2))
			 0))






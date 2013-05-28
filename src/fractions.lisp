
(in-package #:fpoly)

(defclass fpoly-fraction ()
  ((dividend :reader frac-dividend :initarg :dividend)
   (divisor :reader frac-divisor :initarg :divisor)
   (q :reader frac-q :initarg :q)
   (r :reader frac-r :initarg :r)))

(defun make-frac (dividend divisor)
  (multiple-value-bind (q r) (fpoly-div dividend divisor)
	(make-instance 'fpoly-fraction
				   :dividend dividend
				   :divisor divisor
				   :q q
				   :r r)))

(defmethod print-object ((frac fpoly-fraction) stream)
  (if *print-escape*
	  (format stream "#{(~A) / (~A)}" (frac-dividend frac) (frac-divisor frac))
	  (format stream "(~A) / (~A)" (frac-dividend frac) (frac-divisor frac))))





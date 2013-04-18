
(in-package #:fpoly)

(define-condition fpoly-condition (error)
  ()
  (:documentation "Parent error case for all conditions thrown by fpoly"))

(define-condition fpoly-error (fpoly-condition)
  ((place :reader fpoly-error-place :initarg :place)
   (data :reader fpoly-error-data :initarg :data))
  (:report (lambda (condition stream)
			 (format stream
					 "fpoly error ~A at ~A."
					 (fpoly-error-data condition)
					 (fpoly-error-place condition))))
  (:documentation "This error is thrown when an error occurs in FPOLY"))




		   

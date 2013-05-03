

(defpackage #:fpoly
  (:use #:cl)  
  (:export
           ;; fpoly
           #:make-fpoly
		   #:make-monomial
		   #:docoeffs
		   #:fpoly-vars
		   #:fpoly-degree
		   #:fpoly-coeffs
		   #:fpoly-coeff
		   #:parse-fpoly
		   
		   ;; operators
		   #:fpoly-add
		   #:fpoly-incf
		   #:fpoly-sub
		   #:fpoly-mul
		   #:fpoly-div
		   #:fpoly-eql
		   #:fpoly-mod
		   #:fpoly-expt
		   #:fpoly-eval
		   #:fpoly-copy
		   #:fpoly-substitute
		   #:fpoly-simplify
		   #:fpoly-sum
		   
		   ;; matrices
		   #:make-matrix
		   #:mat-entry
		   #:doentries
		   #:mapmatrix
		   #:matrix-modulo
		   #:eval-matrix
		   #:choose-primes
		   #:choose-bindings
		   #:generate-matrices
		   #:print-matrices
		   #:ffge
		   #:det
		   #:fpoly-det
		   
		   ;; interpolate
		   #:lagrange-interpolate
		   #:lagrange-interpolate-matrix

		   ;; parsing
		   #:parse-fpoly
		   #:parse-fpoly-string
		   #:read-vector
		   #:read-matrix
		   ))


(defpackage #:fpoly-ffi
  (:use #:cl #:cffi)
  (:export #:%ffge))




(defpackage #:fpoly
  (:use #:cl)  
  (:export
           ;; fpoly
           #:make-fpoly
		   #:docoeffs
		   #:fpoly-vars
		   #:fpoly-degree
		   #:fpoly-coeffs
		   #:fpoly-coeff

		   ;; operators
		   #:fpoly-add
		   #:fpoly-sub
		   #:fpoly-mul
		   #:fpoly-div
		   #:fpoly-eql
		   #:fpoly-mod
		   #:fpoly-eval
		   #:fpoly-copy

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
		   ))

		   


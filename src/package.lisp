

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
		   #:load-matrix
		   #:load-vector
		   
		   ;; interpolate
		   #:lagrange-interpolate
		   #:lagrange-interpolate-matrix
		   ))


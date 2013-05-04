

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
		   #:mat-size
		   #:doentries
		   #:mapmatrix
		   #:mat-list
		   #:list-mat
		   #:matrix-modulo
		   #:eval-matrix
		   #:choose-primes
		   #:choose-bindings
		   #:generate-matrices
		   #:print-matrices
		   #:print-matrix-mma
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
		   #:load-vector
		   #:load-matrix 
		   ))


(defpackage #:fpoly-ffi
  (:use #:cl #:cffi)
  (:export #:load-fpoly
		   #:%fpoly-open
		   #:%fpoly-close
		   #:%make-fpoly
		   #:%ffge))




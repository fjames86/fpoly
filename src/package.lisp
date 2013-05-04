

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
		   #:combine-mat-vec
		   #:destructure-matrix
		   
		   #:matrix-modulo
		   #:eval-matrix

		   #:choose-primes
		   #:choose-bindings
		   #:generate-matrices
		   
		   #:print-matrices
		   #:print-matrix-mma

		   #:echelon
		   #:det
		   #:fpoly-det
		   #:invert
		   
		   ;; interpolate
		   #:lagrange-interpolate
		   #:lagrange-interpolate-matrix

		   ;; parsing
		   #:parse-fpoly
		   #:parse-fpoly-string
		   #:read-vector
		   #:read-matrix
		   #:load-matrix 
		   ))


(defpackage #:fpoly-ffi
  (:use #:cl #:cffi)
  (:export #:load-fpoly
		   #:%fpoly-open
		   #:%fpoly-close
		   #:%make-fpoly
		   #:%ffge))




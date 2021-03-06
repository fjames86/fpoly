

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
		   #:fpoly-size
		   
		   ;; operators
		   #:fpoly-add
		   #:fpoly-incf
		   #:fpoly-sub
		   #:fpoly-mul
		   #:fpoly-div
		   #:fpoly-eql
		   #:fpoly-zerop
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
		   #:solve-system
		   #:simple-solve
		   #:direct-solve
		   
		   #:gen-test-system
		   #:def-test-system

		   #:print-matrices
		   #:print-matrix-mma
		   
		   #:echelon
		   #:fpoly-echelon
		   #:det
		   #:invert
		   #:lu-decompose
		   
		   ;; interpolate
		   #:lagrange-interpolate
		   #:lagrange-interpolate-matrix

		   
		   ;; parsing
		   #:parse-fpoly
		   #:parse-fpoly-string
		   #:enable-fpoly-syntax
		   #:disable-fpoly-syntax
		   #:read-matrix
		   #:load-matrix

		   ;; compiler
		   #:defpoly

		   ;; fractions
		   #:make-frac

		   ;; repl
		   #:fpoly-repl
		   ))

;; CFFI interface to C/CUDA library.
(defpackage #:fpoly-ffi
  (:use #:cl #:cffi)
  (:export #:load-fpoly
		   #:%fpoly-open
		   #:%fpoly-close
		   #:%make-fpoly
		   #:%ffge
		   #:%ffge-list
		   #:%det
		   #:%det-list))




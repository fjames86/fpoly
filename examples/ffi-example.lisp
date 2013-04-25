
(defpackage #:ffi-test
  (:use #:cl #:fpoly #:fpoly-ffi))

(in-package #:ffi-test)

;; example of using the foreign function interface to libfpoly

;; currently, only ffge is in the library

(defvar mat (make-array (list 3 3)
						:initial-contents '((10 1 -1) (2 0 2) (13 3 -23))))

(defvar vec (make-array 3
						:initial-contents '(1 -2 33)))

;;; load the library
(load-fpoly "/home/frank/quicklisp/local-projects/fpoly/libfpoly/")

;; lisp reduction operation
(time (ffge mat vec))
;; #2A((10 1 -1) (0 -2 22) (0 0 6))
;; #(1 -22 -26)
;; -> 32,220 processor cycles in my test

;; C reduction operation
(%ffge mat vec)
;; #2A((10 1 -1) (0 -2 22) (0 0 6))
;; #(1 -22 -26)
;; -> 29,860 processor cycles



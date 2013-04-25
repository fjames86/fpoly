
(defpackage #:ffi-test
  (:use #:cl #:fpoly #:fpoly-ffi))

(in-package #:ffi-test)

;; example of using the foreign function interface to libfpoly

;; currently, only ffge is in the library

(defvar mat (make-array (list 3 3)
						:initial-contents '((1 2 3) (4 5 6) (7 8 9))))

(defvar vec (make-array 3
						:initial-contents '(1 2 3)))


;; lisp reduction operation
(time (ffge mat vec))
;; -> 32,220 processor cycles in my test

;; C reduction operation
(%ffge mat vec)
;; -> 29,860 processor cycles



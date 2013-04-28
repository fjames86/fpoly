
(defpackage #:ffi-test
  (:use #:cl #:fpoly #:fpoly-ffi))

(in-package #:ffi-test)

;; example of using the foreign function interface to libfpoly

;; currently, only ffge is in the library

(defvar mat (make-array (list 3 3)
						:initial-contents '((10 1 -1) (2 0 2) (13 3 -23))))

(defvar vec (make-array 3
						:initial-contents '(1 -2 33)))

;;; load the library. provide a path to the library, otherwise it'll
;;; probably moan that it can't find it.
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

;; CUDA Example with %ffge-list calling a CUDA function on the GPU

(defun random-matrix ()
  (let ((m (make-array (list 3 3))))
	(dotimes (i 3)
	  (dotimes (j 3)
		(setf (aref m i j) (- 5 (random 10)))))
	m))

(defun random-vector ()
  (let ((v (make-array 3)))
	(dotimes (i 3)
	  (setf (svref v i) (- 5 (random 10))))
	v))

(defvar mymats (loop for i below 100 collect (random-matrix)))
(defvar myvecs (loop for i below 100 collect (random-vector)))

;; call the GPU function
(time (%ffge-list mymats myvecs))

Evaluation took:
  0.001 seconds of real time
  0.000000 seconds of total run time (0.000000 user, 0.000000 system)
  0.00% CPU
  1,623,529 processor cycles
  32,768 bytes consed


;; do it in Lisp
(time (fpoly::ffge-list mymats myvecs))

Evaluation took:
  0.000 seconds of real time
  0.000000 seconds of total run time (0.000000 user, 0.000000 system)
  100.00% CPU
  469,142 processor cycles
  32,768 bytes consed

;; Seems to be much quicker to do it in Lisp, but that could be due to a number of factors
;; e.g. matrix size, thread/block allocation etc.


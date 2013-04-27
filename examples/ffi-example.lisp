
(defpackage #:ffi-test
  (:use #:cl #:fpoly #:fpoly-ffi))

(in-package #:ffi-test)

;; example of using the foreign function interface to libfpoly

;; currently, only ffge is in the library

(defvar mat (make-array (list 3 3)
						:initial-contents '((10 1 -1) (2 0 2) (13 3 -23))))

(defvar vec (make-array 3
						:initial-contents '(1 -2 33)))

(with-foreign-object (mats :int 9) (with-foreign-object (vecs :int 3) (setf (mem-aref mats :int 0) 10)
														(setf (mem-aref mats :int 1) 1) (setf (mem-aref mats :int 2) -1)
														(setf (mem-aref mats :int 3) 2) (setf (mem-aref mats :int 4) 0)
														(setf (mem-aref mats :int 5) 2) (setf (mem-aref mats :int 6) 13)
														(setf (mem-aref mats :int 7) 3)(setf (mem-aref mats :int 8) -23)
														(setf (mem-aref vecs :int 0) 1) (setf (mem-aref vecs :int 1) -2)
														(setf (mem-aref vecs :int 2) 33) (ffge-list2 mats vecs 1 3)
														(let ((m (make-array (list 3 3))) (v (make-array 3)))
														  (dotimes (i 3) (dotimes (j 3) (setf (aref m i j) (mem-aref mats :int (+ (* i 3) j))))
																   (setf (svref v i) (mem-aref vecs :int i))) (list m v))))


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



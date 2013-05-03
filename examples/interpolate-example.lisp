
(defpackage :fpoly-test
  (:use :cl :fpoly))

(in-package :fpoly-test)

;; find the polynomial in X,Y that goes through the points
;; (X, Y)  = (0, 0)  (0, 1)  (1, 1)
;; P(X, Y) = 1,      2,      3
;; of degree 1, i.e. P(X,Y) = a0 + a1 X + a2 Y
;;

(defvar p1 (lagrange-interpolate '(x y)
								 '((0 0) (0 1) (1 1))
								 '(1 2 3)
								 1))

;; now check it really did get the correct polynomial
(fpoly-eval p1 '((x . 0) (y . 0))) ; = 1
(fpoly-eval p1 '((x . 0) (y . 1))) ; = 2
(fpoly-eval p1 '((x . 1) (y . 1))) ; = 3

;; find the polynomial in X, Y that goes through
;; (X, Y)  = (0, 1) (2, 1) (1, 3) (-2, -1) (-3, 2) (-1, 2)
;; P(X, Y) = -7      3       -10     11      1       -11
;; of degree 2, P(X,Y) = a00 + a10 X + ... + a22 Y^2

(defvar p2 (lagrange-interpolate '(x y)
								 '((0 1) (2 1) (1 3) (-2 -1) (-3 2) (-1 2))
								 '(-7 3 -10 11 1 -11)
								 2))

;; check ....
(fpoly-eval p2 '((x . 0) (y . 1))) ;; = -7
(fpoly-eval p2 '((x . 2) (y . 1))) ;; = 3
;; etc ....








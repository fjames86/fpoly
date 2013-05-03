
(in-package #:fpoly)

; 1 + 2x + 3y + 4x^2 + 5xy + 6y^2
(defvar fp1 (make-fpoly '(x y) 2 '(1 2 3 4 5 6)))

; random coefficients up to degree 5
(defvar fp2 (make-fpoly '(x z) 5))

(docoeffs (fp2 coeff powers)
  (setf coeff (- 5 (random 10))))

;; print it out
(princ fp2)

;;

;; square polynomials
(defun fpoly-sq (p) (fpoly-mul p p))

(defvar fp3 (fpoly-add fp1 fp2))

;; make a big polynomial
(defvar fp-big (fpoly-mul (fpoly-sq fp1) (fpoly-sq fp2)))

(princ fp-big)


;; use the read macro
(defvar fp4 #{1 + 2x^2})

(fpoly-expt fp4 2)






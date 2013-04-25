
(in-package #:fpoly-ffi)

(define-foreign-library libfpoly
  (:unix "libfpoly.so")
  (t "libfpoly.so"))

(defun load-fpoly ()
  (use-foreign-library libfpoly))

(defcstruct matrix-t
  (entries :pointer)
  (n :int))

(defcstruct vector-t
  (entries :pointer)
  (n :int))

(defcfun ("ffge" libfpoly-ffge) :void
  (v (:pointer (:struct vector-t)))
  (n :int))

(defun %ffge (mat vec)
  (let ((n (length vec)))
	(with-foreign-object (matrix 'matrix-t)
	  (with-foreign-object (mat-entries :int (* n n))
		(setf (foreign-slot-value matrix 'matrix-t 'entries) vals-array
			  (foreign-slot-value matrix 'matrix-t 'n) n)
		(dotimes (i n)
		  (dotimes (j n)
			(setf (mem-aref mat-entries :int (+ (* i n) j)) (aref mat i j))))
		(with-foreign-object (vect 'vector-t)
		  (with-foreign-object (vect-entries :int n)
			(setf (foreign-slot-value vect 'vector-t 'entries) vect-entries
				  (foreign-slot-value vect 'vector-t 'n) n)
			(dotimes (i n)
			  (setf (mem-aref vect-entries :int i) (svref vec i)))
			;; call the C function
			(libfpoly-ffge matrix vect)
			;; get the return values
			(let ((mat1 (make-array (list n n)))
				  (vec1 (make-array n)))
			  (dotimes (i n)
				(dotimes (j n)
				  (setf (aref mat1 i j) (mem-aref mat-entries :int (+ (* i n) j))))
				(setf (svref vec1 i) (mem-aref vect-entries :int i)))
			  (values mat1 vec1))))))))


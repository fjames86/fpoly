
;;;;
;;;; Foreign function interface to libfpoly (C library of partner functions)
;;;;
;;;; All foreign functions are given the same names as the equivalent Lisp
;;;; routines, with a % character prepended
;;;; 

(in-package #:fpoly-ffi)


(defmacro load-fpoly (&optional search-path)
  "Load the libfpoly library"
  `(progn
	 (define-foreign-library ,(if search-path
								  `(libfpoly :search-path ,search-path)
								  'libfpoly)
	   (:unix "libfpoly.so")
	   (t "libfpoly.so"))
	 (use-foreign-library libfpoly)))

;;; -------------- type definitions -----------------------

;;; --------------- C function definitions -------------------

(defcfun ("ffge" libfpoly-ffge) :void
  (mat :pointer)
  (vec :pointer)
  (n :int))

(defcfun ("ffge_list" libfpoly-ffge-list) :void
  (mats :pointer)
  (vecs :pointer)
  (num :int)
  (n :int))

;;; ---------------- Lisp wrappers -------------------------

(defun maref (row col n)
  (+ (* col n) row))

(defun %ffge (matrix vector)
  "Wrapper for C library FFGE function"
  (let ((n (array-dimension vector 0)))
	(with-foreign-object (mat :int (* n n))
	  (with-foreign-object (vec :int n)
		(dotimes (i n)
		  (dotimes (j n)
			(setf (mem-aref mat :int (maref i j n)) (aref matrix i j)))
		  (setf (mem-aref vec :int i) (svref vector i)))
		(libfpoly-ffge mat vec n)
		(let ((m (make-array (list n n)))
			  (v (make-array n)))
		  (dotimes (i n)
			(dotimes (j n)
			  (setf (aref m i j) (mem-aref mat :int (maref i j n))))
			(setf (svref v i) (mem-aref vec :int i)))
		  (list m v))))))

(defun %ffge-list (matrices vectors)
  (let* ((num (length matrices))
		 (n (array-dimension (car matrices) 0)))
	(with-foreign-object (mats :int (* num n n))
	  (with-foreign-object (vecs :int (* num n))
		(do ((matrices1 matrices (cdr matrices1))
			 (vectors1 vectors (cdr vectors1))
			 (i 0 (1+ i)))
			((or (null matrices1) (null vectors1)))
		  (dotimes (j n)
			(dotimes (k n)
			  (setf (mem-aref mats :int (+ (* i n n) (maref j k n)))
					(aref (car matrices1) j k)))
			(setf (mem-aref vecs :int (+ (* i n) j))
				  (svref (car vectors1) j))))
		(libfpoly-ffge-list mats vecs num n)
		(loop for i below num collect
			 (let ((m (make-array (list n n)))
				   (v (make-array n)))
			   (dotimes (j n)
				 (dotimes (k n)
				   (setf (aref m j k) (mem-aref mats :int (+ (* i n n) (maref j k n)))))
				 (setf (svref v j) (mem-aref vecs :int (+ (* i n) j))))
			   (list m v)))))))

								 
					   


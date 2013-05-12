
;;;;
;;;; Foreign function interface to libfpoly (C library of partner functions)
;;;;
;;;; All foreign functions are given the same names as the equivalent Lisp
;;;; routines, with a % character prepended
;;;; 

(in-package #:fpoly-ffi)


(defmacro load-libfpoly (&optional search-path)
  "Load the libfpoly library"
  `(progn
	 (define-foreign-library ,(if search-path
								  `(libfpoly :search-path ,search-path)
								  'libfpoly)
	   (:unix "libfpoly.so")
	   (:windows "libfpoly.dll")
	   (t "libfpoly.so"))
	 (use-foreign-library libfpoly)))

;(load-libfpoly)

  
;;; -------------- type definitions -----------------------

;;; --------------- C function definitions -------------------

(defcfun ("fpoly_open" libfpoly-open) :void)

(defcfun ("fpoly_close" libfpoly-close) :void)

(defcfun ("make_fpoly" libfpoly-make-fpoly) :pointer
  (nvars :int)
  (vars :pointer)
  (degree :int))

(defcfun ("intern" libfpoly-intern) :pointer
  (str :pointer))

(defcfun ("ffge" libfpoly-ffge) :int
  (mat :pointer)
  (vec :pointer)
  (n :int))

(defcfun ("ffge_list" libfpoly-ffge-list) :void
  (mats :pointer)
  (vecs :pointer)
  (num :int)
  (n :int))

;; if using the GPU library then this might work?
(defcfun ("_Z9ffge_listPiS_ii" libfpoly-ffge-list-gpu) :int
  (mats :pointer)
  (vecs :pointer)
  (num :int)
  (n :int))

(defcfun ("lu_decompose" libfpoly-lu-decompose) :int
  (u :pointer)
  (l :pointer)
  (p :pointer)
  (dd :pointer)
  (matrix :pointer)
  (n :int))

(defcfun ("det" libfpoly-det) :long
  (matrix :pointer)
  (n :int))

(defcfun ("det_list" libfpoly-det-list) :void
  (dets :pointer)
  (mats :pointer)
  (nmats :int)
  (n :int))


;;; ---------------- Lisp wrappers -------------------------

(defun maref (row col n)
  (+ (* col n) row))

(defun %fpoly-open ()
  "Call before any other functions."
  (libfpoly-open))

(defun %fpoly-close ()
  "Free all foreign memory allocted."
  (libfpoly-close))
	
(defun %ffge (matrix vector)
  "Wrapper for C library FFGE function"
  (let ((n (array-dimension vector 0)))
	(with-foreign-object (mat :int (* n n))
	  (with-foreign-object (vec :int n)
		(dotimes (i n)
		  (dotimes (j n)
			(setf (mem-aref mat :int (maref i j n)) (aref matrix i j)))
		  (setf (mem-aref vec :int i) (svref vector i)))
		(unless (zerop (libfpoly-ffge mat vec n))
		  (error "*** %FFGE C call returned error; probably a division by zero"))
		(let ((m (make-array (list n n)))
			  (v (make-array n)))
		  (dotimes (i n)
			(dotimes (j n)
			  (setf (aref m i j) (mem-aref mat :int (maref i j n))))
			(setf (svref v i) (mem-aref vec :int i)))
		  (list m v))))))

(defun %ffge-list (matrices vectors &optional gpu)
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
		
		(if gpu
			(libfpoly-ffge-list-gpu mats vecs num n)
			(libfpoly-ffge-list mats vecs num n))
		
		(loop for i below num collect
			 (let ((m (make-array (list n n)))
				   (v (make-array n)))
			   (dotimes (j n)
				 (dotimes (k n)
				   (setf (aref m j k) (mem-aref mats :int (+ (* i n n) (maref j k n)))))
				 (setf (svref v j) (mem-aref vecs :int (+ (* i n) j))))
			   (list m v)))))))

(defun %det (matrix)
  (let ((n (array-dimension matrix 0)))
	(with-foreign-object (m :int (* n n))
	  (dotimes (i n)
		(dotimes (j n)
		  (setf (mem-aref m :int (maref i j n)) (aref matrix i j))))

	  (libfpoly-det m n))))


(defun %det-list (matrices)
  (let* ((nmats (length matrices))
		 (n (array-dimension (car matrices) 0))
		 (msize (* n n)))
	(with-foreign-object (dets :long nmats)
	  (with-foreign-object (mats :int (* nmats n n))
		(do ((ms matrices (cdr ms))
			 (i 0 (1+ i)))
			((or (null ms) (= i nmats)))
		  (dotimes (j n)
			(dotimes (k n)
			  (setf (mem-aref mats :int (+ (* i msize) (maref j k n)))
					(aref (car ms) j k)))))

		(libfpoly-det-list dets mats nmats n)

		(loop for i below nmats collect
			 (mem-aref dets :int i))))))



								 
					   


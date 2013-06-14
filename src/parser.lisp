

;;;;
;;;; Parser for fpoly objects
;;;; Frank James 2013
;;;;

(in-package :fpoly)

(defun parse-fpoly-simple (stream)
  "Parse a number or polynomial of format <vars> <degree> <coeffs>"
  (let* ((str (read-line stream nil nil))
		 (i (if str (parse-integer str :junk-allowed t) nil)))
	(cond
	  ((null str) nil)
	  (i i)
	  (t (with-input-from-string (s str)
		   (let* ((vars (read s nil nil))
				  (degree (read s nil nil))
				  (coeffs (read s nil nil)))
			 (if (and (listp vars)
					  (numberp degree)
					  (or (listp coeffs) (arrayp coeffs)))
				 (make-fpoly vars degree coeffs)
				 nil)))))))

;;;; -----------------------------------

(defun whitespacep (char)
  "Return the next non-whitespace character"
  (and (characterp char)
	   (member char '(#\space #\newline #\return #\tab)
			   :test #'char-equal)))

(defun read-digits (stream)
  "Read in consequtive digit characters"
  (labels ((rec (acc)
			 (let ((c (read-char stream nil nil)))
			   (cond
				 ((null c) acc)
				 ((or (digit-char-p c)
					  (and (null acc) (member c '(#\+ #\-) :test #'char-equal)))
				  (rec (cons c acc)))
				 (t (unread-char c stream)
					acc)))))
	(concatenate 'string (nreverse (rec nil)))))

(let (prev)
  (defun read-next-word (stream)
	"Parse the next token"
	(if prev
		(prog1 prev (setf prev nil))
		(let ((c (read-char stream nil nil)))
		  (cond
			((null c)
			 ;; eof
			 nil)
			((alpha-char-p c)
			 ;; variable
			 (intern (string-upcase (string c))))
			((whitespacep c)
			 ;; whitespace
			 (read-next-word stream))
			((digit-char-p c)
			 ;; integer
			 (unread-char c stream)
			 (parse-integer (read-digits stream)))
			((member c '(#\+ #\- #\( #\) #\* #\/ #\^ #\}))
			 ;; special characters
			 c)
			(t
			 ;; invalid characters
			 (error 'fpoly-error
					:place "READ-NEXT-WORD"
					:data (format nil "Invalid character ~A encountered in stream." c)))))))

  (defun unread-next-word (word)
	"Allows peeking of tokens"
	(setf prev word)
	word))

(defun parse-fpoly (stream &key recursive closing-brace)
  "Parse an fpoly object from the stream. Keyword args are for internal use only."
  (labels ((build-monomial (acc nterms)
			 (let ((word (read-next-word stream)))
			   (cond
				 ((null word)
				  ;; eof, expecting to find a closing paren?
				  (cond
					(recursive
					 (error 'fpoly-error
							:place "PARSE-FPOLY"
							:data "Unmatched paren found."))
					(closing-brace
					 (error 'fpoly-error
							:place "PARSE-FPOLY"
							:data "EOF before closing brace."))
					(t
					 (if (zerop nterms)
						 0
						 acc))))
				 ((numberp word)
				  (build-monomial (fpoly-mul word acc) (1+ nterms)))
				 ((symbolp word)
				   ;; symbol found, does it have a ^power following it?
				   (let ((next (read-next-word stream))
						 (power 1))
					 (if (and (characterp next)
							  (char-equal next #\^))
						 (let ((digits (read-digits stream)))
						    (if (zerop (length digits))
								 (error 'fpoly-error
										:place "PARSE-FPOLY"
										:data "Integer power expected after ^ sign.")
								  (setf power (parse-integer digits))))
						 (unread-next-word next))
					 (let ((p (make-fpoly word (abs power))))
					    (setf (fpoly-coeff p (abs power)) 1)
						 (build-monomial (fpoly-mul acc (if (< power 0)
															    (make-frac 1 p)
																    p))
										  (1+ nterms)))))
				 ((char-equal word #\*)
				   ;; multiplication
				   (build-monomial acc nterms))
				 ((char-equal word #\/)
				   ;; division
				   (make-frac acc (build-monomial 1 0)))
				 ((char-equal word #\+)
				  ;; addition of new monomial
				  (if (zerop nterms)
					  (build-monomial 1 0)
					  (fpoly-add acc (build-monomial 1 0))))
				 ((char-equal word #\-)
				  ;; subtraction of new monomial
				  (if (zerop nterms)
					  (build-monomial -1 0)
					  (fpoly-add acc (build-monomial -1 0))))
				 ((char-equal word #\()
				  ;; opening paren, parse a new polynomial until it hits a closing paren
				  (build-monomial (fpoly-mul acc
											 (parse-fpoly stream
														  :recursive t
														  :closing-brace closing-brace))
								  (1+ nterms)))
				 ((char-equal word #\))
				  ;; hit a closing paren, expecting to?
				  (if recursive
					  ;; expecting to hit one
					  (if (zerop nterms)
						  0
						  acc)
					  (error 'fpoly-error
							 :place "PARSE-FPOLY"
							 :data "Unexpected closing paren.")))
				 ((char-equal word #\})
				  (if closing-brace
					  ;; looking for a closing brace }
					  ;; this happens if using a reader macro
					  (if (not recursive)
						  (if (zerop nterms)
							  0
							  acc)
						  (error 'fpoly-error
								 :place "PARSE-FPOLY"
								 :data "Unmatched paren."))
					  (error 'fpoly-error
							 :place "PARSE-FPOLY"
							 :data "Invalid character } found.")))
				 (t
				  ;; unmatched case, must be an error
				  (error 'fpoly-error
						 :place "PARSE-FPOLY"
						 :data (format nil "Unable to parse ~A from stream." word)))))))
	(build-monomial 1 0)))

(defun parse-fpoly-string (string)
  "Parse an fpoly object from a string"
  (with-input-from-string (s string)
	(parse-fpoly s)))

;;; create a reader macro for polynomials #{ }


(defvar *previous-readtables* nil
  "A stack which holds the previous readtables that have been pushed
here by ENABLE-FPOLY-SYNTAX.")
 
(defun fpoly-reader (stream char1 char2)
  (declare (ignore char1 char2))
  (let ((p (parse-fpoly stream :closing-brace t)))
	(if (numberp p)
		(make-fpoly nil 0 (list p))
		p)))

(defun %enable-fpoly-syntax ()
  "Internal function used to enable reader syntax and store current
readtable on stack."
  (push *readtable*
        *previous-readtables*)
  (setq *readtable* (copy-readtable))
  (set-dispatch-macro-character #\# #\{ #'fpoly-reader)
  (values))

(defun %disable-fpoly-syntax ()
  "Internal function used to restore previous readtable." 
  (if *previous-readtables*
    (setq *readtable* (pop *previous-readtables*))
    (setq *readtable* (copy-readtable nil)))
  (values))

(defmacro enable-fpoly-syntax ()
  "Enable FPOLY reader syntax."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (%enable-fpoly-syntax)))

(defmacro disable-fpoly-syntax ()
  "Restore readtable which was active before last call to
ENABLE-FPOLY-SYNTAX. If there was no such call, the standard
readtable is used."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (%disable-fpoly-syntax)))

;;;; ------------- matrices

(defun read-vector (stream)
  "Read in a vector of format {poly, poly, ...}"
  (read-until (complement #'whitespacep) stream)
  (let ((open-brace (read-char stream nil nil)))
	(cond
	  ((null open-brace)
	   nil)
	  ((char-equal open-brace #\{)
	   (mapcar (lambda (s)
				 (parse-fpoly-string s))
			   (remove-if (lambda (substr)
							(string-equal substr ","))
						  (string-split (read-until (lambda (c)
													  (char-equal c #\}))
													stream)
										","))))
	  (t (error 'fpoly-error
				:place "READ-VECTOR"
				:data (format nil "Unexpected character ~A found in vector." open-brace))))))

	  
(defun read-matrix (stream)
  "Read in a matrix of format {{entry, entry, ...}, ...}"
  (read-until (complement #'whitespacep) stream)
  (let ((open-brace (read-char stream nil nil)))
	(cond
	  ((null open-brace) nil)
	  ((char-equal open-brace #\{)
	   (labels ((rec (rows)
				  (let ((row (read-vector stream)))
					(read-char stream nil nil)
					(read-until (complement #'whitespacep) stream)
					(let ((c (read-char stream nil nil)))
					  (cond
						((null c)
						 ;; end of file
						 (append rows (list row)))
						((char-equal c #\,)
						 ;; at least one more row
						 (rec (append rows (list row))))
						((char-equal c #\})
						 ;; closing brace
						 (append rows (list row)))
						(t (error 'fpoly-error
								  :place "READ-MATRIX"
								  :data (format nil
												"Unexpected character ~A found whilst reading matrix" c))))))))
		 (rec nil)))
	  (t (error 'fpoly-error
				:place "READ-MATRIX"
				:data (format nil "Unexpected character ~A found whilst reading matrix" open-brace))))))


(defun load-matrix (filename)
  "Read in a matrix from a file. Expects an (n+1) x n matrix."
  (with-open-file (f filename :direction :input)
	(let ((mlist (read-matrix f)))
	  (let ((n (length mlist)))
		(let ((m (make-array (list n (1+ n))
							 :initial-contents mlist)))
		  m)))))

		




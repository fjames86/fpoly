

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
				 ((digit-char-p c)
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
			((member c '(#\+ #\- #\( #\) #\* #\^ #\}))
			 ;; special characters
			 c)			
			(t
			 ;; invalid characters
			 (error "*** read-next-word: Invalid character ~A encountered in stream" c))))))

  (defun unread-next-word (word)
	"Allows peeking of tokens"
	(setf prev word)
	word))

(defun parse-fpoly (stream &key recursive closing-brace)
  "Parse an fpoly object from the stream. The recursive keyword arg is for internal use only"
  (labels ((build-monomial (acc)
			 (let ((word (read-next-word stream)))
			   (cond
				 ((null word)
				  ;; eof, expecting to find a closing paren?
				  (cond
					(recursive
					 (error "*** parse-fpoly: unmatched paren found"))
					(closing-brace
					 (error "*** parse-fpoly: no closing brace"))
					(t acc)))
				 ((numberp word)
				  (build-monomial (fpoly-mul word acc)))
				 ((symbolp word)
				  ;; symbol found, does it have a ^power following it?
				  (let ((next (read-next-word stream))
						(power 1))
					(if (and (characterp next)
							 (char-equal next #\^))
						(setf power (parse-integer (read-digits stream)))
						(unread-next-word next))
					(let ((p (make-fpoly word power)))
					  (setf (fpoly-coeff p power) 1)
					  (build-monomial (fpoly-mul acc p)))))
				 ((char-equal word #\*)
				  ;; multiplication
				  (build-monomial acc))
				 ((char-equal word #\+)
				  ;; addition of new monomial
				  (fpoly-add acc (build-monomial 1)))
				 ((char-equal word #\-)
				  ;; substraction of new monomial
				  (fpoly-sub acc (build-monomial 1)))
				 ((char-equal word #\()
				  ;; opening paren, parse a new polynomial until it hits a closing paren
				  (build-monomial (fpoly-mul acc
											 (parse-fpoly stream
														  :recursive t
														  :closing-brace closing-brace))))
				 ((char-equal word #\))
				  ;; hit a closing paren, expecting to?
				  (if recursive
					  ;; expecting to hit one
					  acc
					  (error "*** parse-fpoly: unexpected closing paren")))
				 ((char-equal word #\})
				  (if closing-brace
					  ;; looking for a closing brace }
					  ;; this happens if using a reader macro
					  (if (not recursive)
						  acc
						  (error "*** parse-fpoly: unmatched paren"))
					  (error "*** parse-fpoly: invalid character }")))
				 (t
				  ;; unmatched case, must be an error
				  (error "*** parse-fpoly: unable to parse from stream"))))))
	(build-monomial 1)))

(defun parse-fpoly-string (string)
  "Parse an fpoly object from a string"
  (with-input-from-string (s string)
	(parse-fpoly s)))

;;; create a reader macro for polynomials #{ }

(set-macro-character #\} (get-macro-character #\)))
 
(set-dispatch-macro-character
 #\# #\{
 (lambda (stream char1 char2)
   (declare (ignore char1 char2))
   (parse-fpoly stream :closing-brace t)))


							  

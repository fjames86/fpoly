
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
  (and (characterp char)
	   (member char '(#\space #\newline #\return #\tab)
			   :test #'char-equal)))

(defun read-digits (stream)
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
			((member c '(#\+ #\- #\( #\) #\* #\^))
			 ;; special characters
			 c)
			(t
			 ;; invalid characters
			 (error "*** read-next-word: Invalid character ~A encountered in stream" c))))))

  (defun unread-next-word (word)
	(setf prev word)
	word))

(defun parse-fpoly (stream &key recursive)
  (labels ((build-monomial (acc)
			 (let ((word (read-next-word stream)))
			   (cond
				 ((null word)
				  ;; eof, expecting to find a closing paren?
				  (if recursive
					  (error "*** parse-fpoly: unmatched paren found")
					  acc))
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
				  (build-monomial (fpoly-mul acc (parse-fpoly stream :recursive t))))
				 ((char-equal word #\))
				  ;; hit a closing paren, expecting to?
				  (if recursive
					  ;; expecting to hit one
					  acc
					  (error "*** parse-fpoly: unexpected closing paren")))
				 (t
				  ;; unmatched case, must be an error
				  (error "*** parse-fpoly: unable to parse from stream"))))))
	(build-monomial 1)))

(defun parse-fpoly-string (string)
  (with-input-from-string (s string)
	(parse-fpoly s)))



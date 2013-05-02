
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

;; -----------------------------------------


(defun remove-whitespace (string)
  "Remove all whitespace characters"
  (remove-if (lambda (c)
			   (some (lambda (d)
					   (char-equal c d))
					 '(#\space #\newline #\return #\tab)))
			 string))

(defun insert-mul-sign (string &key long-names)
  "Insert * signs between numbers and symbols"
  (with-output-to-string (s)
	(labels ((rec (index prev)
			   (if (= index (length string))
				   s
				   (let ((c (char string index)))
					 (cond
					   ((and (digit-char-p prev)
							 (alpha-char-p c))
						(princ #\* s)
						(princ c s)
						(rec (1+ index)
							 c))
					   ((and (not long-names)
							 (> index 0)
							 (alpha-char-p prev)
							 (alpha-char-p c))
						(princ #\* s)
						(princ c s)
						(rec (1+ index)
							 c))
					   (t (princ c s)
						  (rec (1+ index)
							   c)))))))
	  (rec 0 #\a)))) 

(defun insert-powers (string)
  "Insert ^1 to variables if not present"
  (with-output-to-string (s)
	(labels ((rec (index prev)
			   (if (= index (length string))
				   (if (alpha-char-p prev)
					   (princ "^1" s))
				   (let ((c (char string index)))
					 (if (and (alpha-char-p c)
							  (alpha-char-p prev))
						 (princ "^1" s))
					 (princ c s)
					 (rec (1+ index) c)))))
	  (rec 0 #\0))
	s))

(defun insert-coeff (string)
  "Insert a 1* to the front of the string"
  (with-output-to-string (s)
	(if (alpha-char-p (char string 0))
		(princ "1*" s))
	(princ string s)
	s))

	 
(defun try-parse-integer (string)
  "Parse an integer out of a string, returning nil if unable to"
  (let* ((s (string-trim " " string))
		 (len (length s)))
	(multiple-value-bind (int n) (parse-integer s :junk-allowed t)
	  (if (and int (= n len))
		  int
		  nil))))

(defun parse-monomial (string)
  "Format [coeff][*[var][^power]...], coeff=power=1 by default"
  (let ((tokens (string-split (insert-mul-sign (insert-powers (insert-coeff (remove-whitespace string))))
							  " ^*")))
	(let ((coeff (try-parse-integer (car tokens)))
		  (bindings (mapcar (lambda (var-power)
							  (cons (intern (string-upcase (cadr var-power)))
									(try-parse-integer (cadddr var-power))))
							(group-by (cdr tokens) 4))))
	  (let ((vars (mapcar #'car bindings))
			(powers (mapcar #'cdr bindings)))
		(let ((p (make-fpoly vars (reduce #'+ powers))))
		  (setf (apply #'fpoly-coeff p powers) coeff)
		  p)))))

(defun parse-fpoly (stream)
  "Parse a polynomial from the stream"
  (let ((string (loop for line = (read-line stream nil nil)
					 while line collecting line into lines
					 return (apply #'concatenate 'string lines))))
	(let ((monomial-substrs (string-split string "()")))
	  monomial-substrs)))





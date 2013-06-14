


(in-package #:fpoly)

(defparameter *top-level-env* nil)

(defun lambda? (expr)
  (and (listp expr) (eq (car expr) 'lambda)))

(defun repl-quit ()
  (format t "Bye~%")
  '(quit))

(defun repl-div (a b)
  (multiple-value-bind (q r) (fpoly-div a b)
	(list q r)))

(defun lookup (sym env)
  (let ((res (assoc sym env)))
	(if res
		(values (cdr res) t)
		(gethash sym *top-level-env*))))

(defun repl-eval (expr env)
  (cond
	((null expr) nil)
	((symbolp expr)
	 (multiple-value-bind (val found) (lookup expr env)
	   (if found
		   val
		   (error 'fpoly-error
				  :place "REPL-EVAL"
				  :data (format nil "Symbol ~A is not in the current environment" expr)))))
	((atom expr) expr)
	(t (destructuring-bind (proc-sym . args) expr
		 (cond
		   ((eq proc-sym 'quote)
			(car args))
		   ((eq proc-sym 'if)
			(let ((test (repl-eval (car args) env)))
			  (if test
				  (repl-eval (cadr args) env)
				  (repl-eval (caddr args) env))))
		   ((eq proc-sym 'def)
			(if (symbolp (car args))
				(destructuring-bind (name val) args
				  (setf (gethash name *top-level-env*) (repl-eval val nil))
				  name)
				(destructuring-bind ((name . params) . body) args
				  (setf (gethash name *top-level-env*)
						(list 'lambda params body env))
				  name)))
		   ((eq proc-sym 'block)
			(repl-eval-exprs args env))
		   ((eq proc-sym 'let)
			(destructuring-bind (bindings . body) args
			  (repl-eval-exprs body
							   (append (mapcar (lambda (binding)
												 (if (symbolp binding)
													 (cons binding nil)
													 (cons (car binding)
														   (repl-eval (cadr binding) env))))
											   bindings)
									   env))))
		   ((eq proc-sym 'lambda)
			(destructuring-bind (params . body) args
			  (list 'lambda params body env)))
		   (t
			(let ((proc (repl-eval proc-sym env)))
			  (repl-apply proc
						  (mapcar (lambda (arg)
									(repl-eval arg env))
								  args)))))))))

(defun repl-eval-exprs (body env)
  (let ((val (repl-eval (car body) env)))
	(if (null (cdr body))
		val
		(repl-eval-exprs (cdr body) env))))

(defun repl-apply (proc arglist)
  (if (lambda? proc)
	  (destructuring-bind (params body env) (cdr proc)
		(repl-eval-exprs body (append (mapcar #'cons params arglist) env)))
	  (apply proc arglist)))

(defun repl-quit ()
  (format t "Bye~%")
  '(quit))

(defun base-env ()
  (let ((ht (make-hash-table)))
	(mapc (lambda (var val)
			(setf (gethash var ht) val))
		  '(+ - * / expt
			= > >= < <=
			not t nil
			quit
			car cdr list)
		  (list
		   #'fpoly-add #'fpoly-sub #'fpoly-mul #'repl-div #'fpoly-expt
		   #'= #'> #'>= #'< #'<=
		   #'not t nil
		   #'repl-quit
		   #'car #'cdr #'list))
	ht))

(defun fpoly-repl ()
  (setf *top-level-env* (base-env))
  (enable-fpoly-syntax)
  (unwind-protect 
	   (do ((counter 0 (1+ counter))
			(end nil))
		   (end)
		 (format t "~&%i~A> " counter)
		 (force-output)

		 (let ((input (handler-case (read nil '(quit))
						(error (err)
						  (declare (ignore err))
						  (format t "~&*** Read error~%")
						  nil))))
		   (handler-case (let ((result (repl-eval input nil)))
						   (if (and (listp result) (eq (car result) 'quit))
							   (setf end t)
							   (format t "~&%o~A> ~S~%" counter result)))
			 (fpoly-error (err)
			   (format t "~&*** ~A~%" (fpoly-error-data err)))
			 (error (err)
			   (format t "~&*** ~A~%" err)))))
	(disable-fpoly-syntax))
  nil)





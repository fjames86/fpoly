
(in-package #:fpoly)

(defun factorial (n)
  (labels ((rec (n acc)
			 (if (= n 0)
				 acc
				 (rec (1- n) (* acc n)))))
    (rec n 1)))

(defun ncr (n k)
  (/ (factorial n)
     (* (factorial k) (factorial (- n k)))))

;;; ------- various utilities for manipulating powers and vars -----------

(defun test-list (test source-list result-list)
  "Utility to find matching value in paired lists"
  (labels ((rec (source-list result-list)
			 (if (or (null source-list) (null result-list))
				 (values nil nil)
				 (let ((result (funcall test (car source-list))))
				   (if result
					   (values (car result-list) t)
					   (rec (cdr source-list) (cdr result-list)))))))
	(rec source-list result-list)))


(defun merge-powers (vars1 powers1 vars2 powers2)
  "add all the powers together"
  (labels ((rec (bindings1 bindings2 acc)
			 (cond
			   ((null bindings1)
				(append acc bindings2))
			   ((null bindings2)
				(append acc bindings1))
			   (t
				(let* ((var (caar bindings1))
					   (val (cdar bindings1))
					   (pair (assoc var bindings2)))
				  (if pair
					  (rec (cdr bindings1) 
						   (remove-if (lambda (b)
										(eq (car b) var))
									  bindings2)
						   (append acc
								   (list (cons var (+ val (cdr pair))))))
					  (rec (cdr bindings1)
						   bindings2
						   (append acc (list (car bindings1))))))))))
    (mapcar #'cdr
			(rec (mapcar #'cons vars1 powers1)
				 (mapcar #'cons vars2 powers2)
				 nil))))

(defun merge-vars (vars1 vars2)
  (labels ((rec (vars1 vars2 acc)
			 (cond
			   ((null vars1)
				(append acc vars2))
			   ((null vars2)
				(append acc vars1))
			   (t
				(let* ((var (car vars1))
					   (pair (find var vars2)))
				  (if pair
					  (rec (cdr vars1)
						   (remove-if (lambda (b)
										(eq b var))
									  vars2)
						   (append acc (list var)))
					  (rec (cdr vars1)
						   vars2
						   (append acc (list var)))))))))
    (rec vars1 vars2 nil)))

(defun project-powers (source-vars source-powers target-vars)
  "Project the bindings down onto the vars, which must form a sub-basis of the bindings"
  (labels ((rec (source-vars source-powers acc)
			 (if (or (null source-vars) (null source-powers))
				 acc
				 (let ((var (find (car source-vars) target-vars))
					   (power (car source-powers)))
				   (cond
					 (var
					  (rec (cdr source-vars)
						   (cdr source-powers)
						   (cons power acc)))
					 ((zerop power)
					  (rec (cdr source-vars)
						   (cdr source-powers)
						   acc))
					 (t nil))))))
	(nreverse (rec source-vars source-powers nil))))

(defun shuffle-power-order (source-vars source-powers target-vars)
  "Switch power order around, e.g. from (x=1, y=2) -> (y=2, x=1)"
  (mapcar (lambda (var)
			(multiple-value-bind (power found) (test-list (lambda (x) (eq x var))
														 source-vars source-powers)
			  (if found
				  power
				  (error "*** shuffle-power-order: ~A not found in source vars" var))))
		  target-vars))


		 


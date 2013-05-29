
(in-package #:fpoly)


(defun fpoly-repl ()
  (do ((counter 0 (1+ counter))
	   (end nil))
	  (end)
	(format t "~&%i~A> " counter)
	(force-output)
	(let ((command (read nil nil)))
	  (if (null command)
		  (format t "bye~%")
		  (cond
			((and (symbolp command) (eq command 'quit))
			 (setf end t)
			 (format t "bye~%"))
			(t (format t "~&%o~A> ~A~%" counter command)))))))



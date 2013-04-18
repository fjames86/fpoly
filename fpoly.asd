
(in-package :asdf)

(defsystem #:fpoly
  :name "F-POLY"
  :author "Frank James <frank.a.james@gmail.com>"
  :version "1"
  :maintainer "Frank James <frank.a.james@gmail.com>"
  :licence "Lesser Lisp General Public License (LLGPL)"
  :description "Common Lisp Polynomial Package"
  :long-description "Fpoly is a package for manipulation of polynomials"

  :components
  ((:module
    :src 
    :components ((:file "package")
				 (:file "condition")
				 (:file "utils" :depends-on ("package" "condition"))
				 (:file "fpoly" :depends-on ("package" "utils" "condition"))
				 (:file "operators" :depends-on ("package" "utils" "fpoly" "condition"))
				 (:file "matrices" :depends-on ("package" "utils"
												"fpoly" "operators" "condition"))
				 (:file "interpolate" :depends-on ("package" "utils" "fpoly"
												   "operators" "matrices" "condition"))))))






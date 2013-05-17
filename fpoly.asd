
(in-package :asdf)

(defsystem #:fpoly
  :name "F-POLY"
  :author "Frank James <frank.a.james@gmail.com>"
  :version "1"
  :maintainer "Frank James <frank.a.james@gmail.com>"
  :licence "Lisp Lesser General Public License (LLGPL)"
  :description "Common Lisp Polynomial Package"
  :long-description "Fpoly is a package for manipulation of polynomials"

  :components
  ((:module
    :src 
    :components ((:file "package")
				 (:file "conditions" :depends-on ("package"))
				 (:file "utils" :depends-on ("package" "conditions"))
				 (:file "fpoly" :depends-on ("package" "utils" "conditions"))
				 (:file "parser" :depends-on ("package" "utils" "fpoly" "conditions"))
				 (:file "operators" :depends-on ("package" "utils" "fpoly" "conditions"))
				 (:file "matrices" :depends-on ("package" "utils"
												"fpoly" "operators" "conditions"))
				 (:file "interpolate" :depends-on ("package" "utils" "fpoly"
												   "operators" "matrices" "conditions"))
				 (:file "interface" :depends-on ("package"))
				 (:file "compiler" :depends-on ("package" "utils" "fpoly" "parser"
												"operators")))))

  :depends-on (:cffi))







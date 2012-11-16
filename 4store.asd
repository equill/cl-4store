;;; ==============================

(defsystem :4store
  :author  "Patrick D. Logan"
  :maintainer "James Fleming"
  :license "BSD" 
  :description "4store"
  :version "0.0.1"
  :depends-on (:drakma)
  :serial t
  :components ((:file "package")
               (:file "4store-specials")
               (:file "4store-utils")
               (:file "4store")))

(defsystem :4store-tests
  :author "James Fleming"
  :license "BSD" 
  :depends-on (:4store
	       :cl-ppcre
	       :fiveam)
  :serial t
  :components ((:file "test-specials")
               (:file "tests")))

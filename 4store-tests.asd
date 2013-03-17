(defsystem :4store-tests
           :description "Test suite for 4store"
           :version "0.0.1"
           :author "James Fleming"
           :license "BSD" 
           :depends-on (:4store
                         :fiveam
                         :cl-ppcre)
           :components ((:file "defpackage-tests")
                        (:file "4store-test-specials")
                        (:file "4store-tests"))
           :serial t)

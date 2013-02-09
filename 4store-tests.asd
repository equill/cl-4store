(defsystem :4store-tests
  :description "Test suite for 4store"
  :version "0.0.1"
  :depends-on (:4store)
  :components ((:file "4store-test-specials")
               (:file "4store-tests"))
  :serial t)

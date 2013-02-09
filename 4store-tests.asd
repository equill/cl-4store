(defsystem :4store-tests
  :description "Test suite for 4store"
  :version "0.0.1"
  :depends-on (:4store)
  :components ((:file "test-specials")
               (:file "tests"))
  :serial t)

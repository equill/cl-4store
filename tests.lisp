(in-package :4store-tests)

(defun test-insert-triple ()
  (4store::insert-triple
   "http://localhost:8080/"
   "demo"
   "<ox-subjects:foo>"
   "<ox-predicates:bar>"
   "plaintext"))


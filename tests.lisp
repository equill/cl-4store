(in-package :4store-tests)

(defun test-insert-triples ()
  (4store::insert-triples
   "http://localhost:8080/"
   "demo"
   '(("<ox-subjects:foo>" "<ox-predicates:bar>" "plaintext")
     ("<ox-subjects:first-hash>" "<rdfs:label>" "Comment on the primacy of labels")
     ("<ox-subjects:first-hash>" "<rdfs:creator>" "Name of the creator"))))


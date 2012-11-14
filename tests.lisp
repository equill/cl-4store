(in-package :4store-tests)

(defparameter *base-uri* "http://localhost:8080/")
(defparameter *graph-name* "demo")

(fiveam:def-suite 4store-tests)
(fiveam:in-suite 4store-tests)

(fiveam:test (test-status)
	     ;; Can we get a sane status reply from the server?
	     (fiveam:is
	      (= 200 (4store:sparql-server-status-request *base-uri*))))

(fiveam:test (test-insert-triples)
	     ;; Can we store a set of triples?
	     (fiveam:is
	      (cl-ppcre:scan "200 added successfully"
		     (4store:insert-triples
		      *base-uri*
		      *graph-name*
		      '(("<ox-subjects:foo>" "<ox-predicates:bar>" "plaintext")
			("<ox-subjects:first-hash>" "<rdfs:label>" "Comment on the primacy of labels")
			("<ox-subjects:first-hash>" "<rdfs:creator>" "Name of the creator"))))))


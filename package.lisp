(defpackage :4store
  (:use :common-lisp )
  (:import-from :fare-matcher
		:_)
  (:export sparql-query
	   sparql-server-put-data-request
	   insert-triples
	   delete-triple
	   delete-graph
	   ;; Subject to deprecation
	   ;; - these may not belong here.
	   sparql-server-status-request
	   get-triples-list))

(defpackage :4store-tests
  (:use :common-lisp))


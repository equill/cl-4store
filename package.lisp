(defpackage :4store
  (:use :common-lisp )
  (:import-from :fare-matcher
		:_)
  (:export sparql-query
	   sparql-server-put-data-request
	   ;; Subject to deprecation
	   ;; - these may not belong here.
	   sparql-server-status-request
	   get-triples-list))

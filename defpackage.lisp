(defpackage :4store
  (:use :common-lisp )
  (:export sparql-query
           get-triples-list
           sparql-server-put-data-request
           sparql-update
           insert-triples
           delete-triples
           delete-all-triples
           delete-graph
           ;; Subject to deprecation
           ;; - these may not belong here.
           sparql-server-status-request))

(defpackage :4store-tests
  (:use :common-lisp))


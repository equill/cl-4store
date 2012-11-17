;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file was originally workspace.lisp and presented as a collection of
;; 'workspace' code for exploring a 4store RDF knowledgebase using Steel Bank
;; Common Lisp.
;;
;; We forked and have begun adapting it to be a more asdf friendly and by proxy
;; Quicklisp installable.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Copyright (c) 2011, Patrick D. Logan
;; All rights reserved.
;;
;; See COPYING for more information. The license is the "new and
;; simplified BSD license" See
;; http://www.opensource.org/licenses/bsd-license
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:4store)  

;;;; Currently untested, as I've no particular expectation of using this.
(defun sparql-server-put-data-request (server-url graphname filepath)
  "Perform an HTTP put request with the data contained in a file.
  Arguments:
  - the base url pathname of the SPARQL server
  - relative URL component (i.e, the graph name)
  - the path to the file
  Assumes that the input file is valid RDF/XML"
  (let ((drakma:*text-content-types* *4store-text-content-types*))
    (drakma:http-request (concatenate 'string server-url "data/" graphname)
                         ;; Tell 4store to replace the entire graph:
                         :method :put
                         :content-type "application/rdf+xml"
                         :content filepath
                         ;; Force drakma to compute the content-length instead of
                         ;; using chunked encoding:
                         :content-length t)))

(defun sparql-server-status-request (server-url)
  "Returns the numeric HTTP status code from the server.
If all is well, the return code will be 200 (for OK)."
  (nth-value 1 (drakma:http-request (concatenate 'string server-url "status"))))

(defun sparql-query (server-url query &key (method :get) (accept "sparql"))
  "Send a SPARQL query to the server, and return the result.
Expects a valid SPARQL query for its second argument, in the form of a text string.
Uses GET by default, but the :method keyword argument can be used to force POST, PUT, DELETE or whatever other method tickles your fancy.
The :accept keyword allows you to specify which return format to request from 4store:
- \"sparql\": application/sparql-results+xml (default)
- \"text\": text/tab-separated-values (more efficient)
- \"json\": application/sparql-results+json"
  (let ((drakma:*text-content-types* *4store-text-content-types*))
    (drakma:http-request (concatenate 'string server-url "sparql/")
			 :method method
			 :accept accept
			 :parameters `(("query" . ,query)))))

(defun get-triples-list (server-url)
  "Retrieves all triples in the store.
Useful for smoke-testing; use with caution, because it returns _everything_."
  (sparql-query server-url "select ?subject ?predicate ?object
where { ?subject ?predicate ?object }"))

(defun sparql-update (server-url graph data &key (method :post))
  "Send a SPARQL update request to the server, and return the result.
Expects a valid SPARQL query for its second argument, in the form of a text string.
Uses GET by default, but the :method keyword argument can be used to force POST, PUT, DELETE or whatever other method tickles your fancy."
  (drakma:http-request (concatenate 'string server-url "data/")
		       :method method
		       :parameters `(("data" . ,data)
				     ("graph" . ,graph)
				     ("mime-type" . "application/x-turtle"))))

(defun insert-triples (server-url graph triples)
  "Inserts a list of triples into the connected store.
The 'triples argument is expected to be a list of proper lists containing subject, predicate and object"
  (sparql-update server-url
		 graph
		 (with-output-to-string
		   (outstr)
		   (mapcar #'(lambda (triple)
			       (format outstr "~A ~A ~A .~%"
				       (first triple)
				       (second triple) (quote-plaintext (third triple))))
			   triples)
		   outstr)))

(defun delete-triple (server-url graph subject predicate object)
  "Remove one triple from the nominated graph, as per the SPARQL 1.1 spec:
http://www.w3.org/TR/sparql11-update/#deleteData"
  (drakma:http-request
    (concatenate 'string server-url "update/")
    :method :post
    :parameters `(("update" . ,(format nil "DELETE DATA { GRAPH ~A { ~A ~A ~A } }"
                                       (quote-plaintext graph)
                                       subject
                                       predicate
                                       (quote-plaintext object))))))

(defun delete-graph (server-url graph-name)
  "Deletes the identified graph.
Reference command:
curl -X DELETE 'http://localhost:8000/data/?graph=http%3A%2F%2Fexample.com%2Fdata'"
  (drakma:http-request (concatenate 'string server-url "data/?graph=" graph-name)
		       :method :delete))


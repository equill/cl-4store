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

(defun sparql-server-put-data-request (server-url content-data-pathname url-data-component)
  (declare (string url-data-component))
  (wrapped-text-context-request
   (drakma:http-request (render-url-components server-url "data/" url-data-component)
                        :method :put
                        :content content-data-pathname
                        :content-type "application/rdf+xml" :content-length t)))

(defun sparql-server-status-request (server-url)
  (nth-value 1 (drakma:http-request (render-url-components server-url "status"))))

(defun sparql-query (server-url query &key (method :get))
  "Send a SPARQL query to the server, and return the result.
Expects a valid SPARQL query for its second argument, in the form of a text string.
Uses GET by default, but the :method keyword argument can be used to force POST, PUT, DELETE or whatever other method tickles your fancy."
  (let ((drakma:*text-content-types* *4store-text-content-types*))
    (drakma:http-request (render-url-components server-url "sparql/")
			 :method method
			 :parameters `(("query" . ,query)))))

(defun get-triples-list (server-url)
  "Retrieves all triples in the store.
Useful for smoke-testing; use with caution, because it returns _everything_."
  (sparql-query server-url "select ?subject ?predicate ?object
where { ?subject ?predicate ?object }"))

(defun foaf-person-construct-request (&key (server-url *4store-base-url*))
  (sparql-query server-url (gethash :foaf-person-construct *4store-query-cache*) :method :post))

(defun rdfs-class-select-request (&key (server-url *4store-base-url*))
  (sparql-query server-url (gethash :rdfs-class-select *4store-query-cache*) :method :post))

(defun sparql-update (server-url graph data &key (method :post))
  "Send a SPARQL update request to the server, and return the result.
Expects a valid SPARQL query for its second argument, in the form of a text string.
Uses GET by default, but the :method keyword argument can be used to force POST, PUT, DELETE or whatever other method tickles your fancy."
  (drakma:http-request (render-url-components server-url "data/")
		       :method method
		       :parameters `(("data" . ,data)
				     ("graph" . ,graph)
				     ("mime-type" . "application/x-turtle"))))

(defun angle-bracket-delimited-p (str)
  "Detects whether a string is delimited by angle-brackets."
  (cl-ppcre:scan "^<[:\\w]+>$" str))

(defun quote-plaintext (str)
  "Wraps the string in quotes if it isn't delimited by angle-brackets.
Intended for the object of a triple on insertion."
  (if (angle-bracket-delimited-p str)
      str
    (cl-ppcre:regex-replace
     "$"
     (cl-ppcre:regex-replace "^" str "\"")
     "\"")))

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

(defun delete-graph (server-url graph-name)
  "Deletes the identified graph.
Reference command:
curl -X DELETE 'http://localhost:8000/data/?graph=http%3A%2F%2Fexample.com%2Fdata'"
  (drakma:http-request (concatenate 'string server-url "data/?graph=" graph-name)
		       :method :delete))

(defun render-parsed-uri-to-string-if (uri-or-string)
  (if (puri:uri-p uri-or-string)
      (with-output-to-string (as-rendered)
        (puri:render-uri uri-or-string as-rendered))
      uri-or-string))

(defun map-uris-to-strings (list)
  (mapcar #'render-parsed-uri-to-string-if list))

(defun render-request-as-xmls (xml-result)
  (cxml:parse xml-result (cxml-xmls:make-xmls-builder)))

(defun foaf-person-construct-extract ()
  (let ((extracted-persons '()))
    (flet ((render-parsed-persons (s p o)
             (push (map-uris-to-strings (list s p o))
                   extracted-persons)))
      (cl-rdfxml:parse-document #'render-parsed-persons  (foaf-person-construct-request)))
    ;; `cl:nreverse' keeps same order as RDF/XML
    (setf extracted-persons (nreverse extracted-persons))))

(defun rdfs-class-select-render-as-xmls (request-data)
  (render-request-as-xmls request-data))

(defun rdfs-class-select-row-data-extract (rendered-xmls)
  (row-extract (data-extract rendered-xmls)))

(defun rdfs-class-select-string-filter (extracted-row-data)
  (remove-if-typep 'string extracted-row-data))

(defun rdfs-class-uri-match (item)
  ;; Underscores in pattern are "don't care" positions.
  ;; The 'uri' variable is the position of interest.
  (fare-matcher:letm (list _ _ _ (list _ _  (list _ _ uri)) _) 
                     item 
                     uri))

(defun rdfs-class-uri-match-map (filtered-data)
  (mapcar #'rdfs-class-uri-match filtered-data))

(defun rdfs-class-select-extract ()
  (rdfs-class-uri-match-map
   (rdfs-class-select-string-filter    
    (rdfs-class-select-row-data-extract
     (rdfs-class-select-render-as-xmls 
      (rdfs-class-select-request))))))


;;; ==============================
;;; EOF

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file was originally workspace.lisp and presented as a collection of
;; 'workspace' code for exploring a 4store RDF knowledgebase using Steel Bank
;; Common Lisp.
;;
;; It has been modified a little since then.

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

;;;; Currently untested
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

(defun tsv-to-lists (tsv)
  "Convert a 4store TSV result string to a list of lisp lists"
  (delete-if #'null
             (with-input-from-string (instr tsv)
               (loop for line = (read-line instr nil nil)
                     while line collect (unless (cl-ppcre:scan "^\\?" line)
                                          (split-sequence:split-sequence #\Tab line))))))

;;;; Currently tested indirectly, via 'get-triples-list
(defun sparql-query (server-url graph return-vars query-params &optional optional-params)
  "Send a SPARQL query to the server, and return the result in TSV format.
  XML and JSON are available as return formats in 4store, but TSV is faster and
  simpler to parse into lists for further processing.
  Arguments:
  - server-url (bare string)
  - graph ID (string containing the URI serving as the graph ID)
  - return-vars: a list of strings naming the return variables. Question-marks
  are prepended automatically.
  - query-params: a list of the triples that comprise the query itself. It's
  currently necessary to manually prepend question-marks to the return-var
  names here.
  - optional-params: a list of optional query-triples. If one or more of the
  return-vars is optional, put the relevant query-triples here. If they can be
  satisfied, the resulting value will be returned; if not, NIL is returned."
  (tsv-to-lists
    (let ((drakma:*text-content-types* *4store-text-content-types*)
          (drakma:*drakma-default-external-format* :utf-8)
          (optional-parameters (if optional-params
                                 (format nil ".~% OPTIONAL { ~{~{~A ~}~^.~%~} } " optional-params)
                                 "")))
      (drakma:http-request
        (concatenate 'string server-url "sparql/")
        :parameters `(("query" .
                       ,(format nil "SELECT DISTINCT ~{?~A ~} WHERE { GRAPH ~A { ~{~{~A ~}~^.~%~}~A } }"
                                return-vars
                                graph
                                query-params
                                optional-parameters))
                      ("output" . "text"))))))

(defun get-triples-list (server-url graph)
  "Retrieves all triples in the store.
  Useful for smoke-testing; use with caution in large stores, because it returns
  _everything_."
  (sparql-query
    server-url
    graph
    '("subject" "predicate" "object")
    '(("?subject" "?predicate" "?object"))))

;;;; Currently tested indirectly, via 'insert-triples
(defun sparql-update (server-url data &key (method :post))
  "Send a SPARQL update request to the server, and return the result.
  Expects a valid SPARQL query for its second argument, in a text string.
  Uses POST by default, but the :method keyword argument can be used to force
  POST, PUT, DELETE or whatever other method tickles your fancy."
  (let ((drakma:*drakma-default-external-format* :utf-8))
    (drakma:http-request (concatenate 'string server-url "update/")
                         :method method
                         :parameters `(("update" . ,data)
                                       ("mime-type" . "application/x-turtle")))))

(defun insert-triples (server-url graph triples)
  "Inserts a list of triples into the store.
  The 'triples argument is expected to be a list of proper lists, each
  containing subject, predicate and object."
  (sparql-update server-url
                 (with-output-to-string
                   (outstr)
                   (format outstr "INSERT DATA { GRAPH ~A { " graph)
                   (mapcar #'(lambda (triple)
                               (format outstr "~A ~A ~A . "
                                       (first triple)
                                       (second triple)
                                       (third triple)))
                           triples)
                   (format outstr "} } ")
                   outstr)))

(defun delete-triples (server-url graph triples)
  "Remove the supplied set of triples from the graph.
  Expects the 'triples argument to be a list of three-element lists.
  If the Object is plain text, it's expected to already be quoted."
  (sparql-update server-url
                 (with-output-to-string
                   (outstr)
                   (format outstr "DELETE DATA { GRAPH ~A { " graph)
                   (mapcar #'(lambda (triple)
                               (format outstr "~A ~A ~A . "
                                       (first triple)
                                       (second triple)
                                       (third triple)))
                           triples)
                   (format outstr "} } ")
                   outstr)))

(defun delete-all-triples (server-url graph)
  "Deletes _all_ triples in the specified graph."
  (let ((triples (get-triples-list server-url graph)))
    (when triples (delete-triples server-url graph triples))))

(defun delete-graph (server-url graph-name)
  "Deletes the identified graph.
  Currently known not to work; despite being semantically correct and receiving
  a success code from the server."
  (drakma:http-request (concatenate 'string server-url "data/")
                       :method :delete
                       :parameters `(("graph" . ,graph-name))))


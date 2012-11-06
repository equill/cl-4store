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

(defun sparql-server-put-data-request (content-data-pathname url-data-component &key (server-url *4store-base-url*))
  (declare (string url-data-component))
  (wrapped-text-context-request
   (drakma:http-request (render-url-components server-url "data/" url-data-component)
                        :method :put
                        :content content-data-pathname
                        :content-type "application/rdf+xml" :content-length t)))

(defun sparql-server-status-request (&key (server-url *4store-base-url*))
  (nth-value 1 (drakma:http-request (render-url-components server-url "status"))))

(defun render-parsed-uri-to-string-if (uri-or-string)
  (if (puri:uri-p uri-or-string)
      (with-output-to-string (as-rendered)
        (puri:render-uri uri-or-string as-rendered))
      uri-or-string))

(defun map-uris-to-strings (list)
  (mapcar #'render-parsed-uri-to-string-if list))

(defun render-request-as-xmls (xml-result)
  (cxml:parse xml-result (cxml-xmls:make-xmls-builder)))

(defun foaf-person-construct-request (&key (server-url *4store-base-url*))
  (wrapped-text-context-request
   (drakma:http-request (render-url-components server-url "sparql/")
                        :method :post  
                        :parameters `(("query" . ,(gethash :foaf-person-construct *4store-query-cache*))))))

(defun foaf-person-construct-extract ()
  (let ((extracted-persons '()))
    (flet ((render-parsed-persons (s p o)
             (push (map-uris-to-strings (list s p o))
                   extracted-persons)))
      (cl-rdfxml:parse-document #'render-parsed-persons  (foaf-person-construct-request)))
    ;; `cl:nreverse' keeps same order as RDF/XML
    (setf extracted-persons (nreverse extracted-persons))))

(defun rdfs-class-select-request (&key (server-url *4store-base-url*))
  (wrapped-text-context-request
   (drakma:http-request (render-url-components server-url "sparql/")
                        :method :post  
                        :parameters `(("query" . ,(gethash :rdfs-class-select *4store-query-cache*))))))

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

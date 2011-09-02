;;; :FILE-CREATED <Timestamp: #{2011-09-02T03:30:06-04:00Z}#{11355} - by MON>
;;; :FILE sbcl-4store/4store.lisp
;;; ==============================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is a collection of 'workspace' code for exploring a
;; 4store RDF knowledgebase using Steel Bank Common Lisp. The code has
;; been used with Steel Bank Common Lisp, but should run without
;; issues in many if not most Common Lisp implementations.
;; 
;; Copyright (c) 2011, Patrick D. Logan
;; All rights reserved.
;;
;; See COPYING for more information. The license is the "new and
;; simplified BSD license" See
;; http://www.opensource.org/licenses/bsd-license
;;
;; For installing 4store on ubuntu see:
;; http://patricklogan.blogspot.com/2011/03/building-4store-on-ubuntu-1010.html
;; 
;; Create a new 4store knowledgebase:
;; 4s-backend-setup sample
;;
;; Start the knowledgebase server as a daemon:
;; 4s-backend sample
;;
;; Start a sparql endpoint http server for the knowledgebase:
;; 4s-httpd  -p 8080 -D sample
;;
;; -D prevents the http server from daemonizing in order to see
;; messages interactively.
;;
;; Status of the (empty) KB is now at http://localhost:8080/status/
;;
;; Load the sample data (an RDF/XML file) using curl (here) or a PUT
;; from lisp (below):
;; curl -v -T organogram-co-2010-10-31-index.rdf 'http://localhost:8080/data/organogram-co-2010-10-31-index'
;; /home/sp/HG-Repos/CL-repo-HG/CL-SYSTEMS/sbcl-4store-GIT
;;
;; The data file comes from
;; http://source.data.gov.uk/data/reference/organogram-co/2010-10-31/index.rdf
;; and other formats are also available there.
;;
;; Install Steel Bank Common Lisp from http://www.sbcl.org/
;;
;; Install quicklisp for finding and managing CL libraries (similar to
;; ruby's gems)
;; http://www.quicklisp.org/

;; (ql:quickload "puri")          ; Working with URIs.
;; (ql:quickload "drakma")        ; An HTTP client.
;; (ql:quickload "cxml")          ; XML parsing.
;; (ql:quickload "fare-matcher")  ; Lisp pattern matching.
;; (ql:quickload "cl-rdfxml")     ; RDF/XML parsing into RDF-specific objects.

;;;; (ql:quickload "cl-utilities")  ; Em, utilities not used currently in this file.
;;;; (ql:quickload "hunchentoot")   ; A web server, but only import a couple URL utilities.

;;; ==============================
(in-package #:4store)

;; Tell drakma to treat these additional types as text rather than
;; binary.
;; (setq drakma:*text-content-types* (list* '("application". "sparql-results+xml")
;;                                          '("application" . "rdf+xml")
;;                                          drakma:*text-content-types*))
(defvar *4store-text-content-types* 
  (list* '("application". "sparql-results+xml")
         '("application" . "rdf+xml")
         drakma:*text-content-types*))

(defvar *4store-base-url* "http://localhost:8080/")

(defvar *4store-query-cache* (make-hash-table))

(setf (gethash :foaf-person-construct *4store-query-cache*)
      "prefix foaf: <http://xmlns.com/foaf/0.1/>
construct {
  ?person 
    a foaf:Person ;
    foaf:name ?name ;
    ?prop ?value .
} where { 
  ?person a foaf:Person ;
    foaf:name ?name ;
    ?prop ?value .                        
}")

(setf (gethash :rdfs-class-select *4store-query-cache*)
"prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
select distinct ?type 
where { 
    ?x a ?type .
} 
order by ?type")


(defmacro wrapped-text-context-request (&rest body)
  `(let ((drakma:*text-content-types* *4store-text-content-types*))
     ,@body))

(defun put-data (content-data-pathname url-data-component &optional (server-url *4store-base-url*))
  "This PUTs the data file at the given URL. 
Equivalalent of: 
 shell> curl -v -T organogram-co-2010-10-31-index.rdf \
       'http://localhost:8080/data/organogram-co-2010-10-31-index'"
  (declare (string url-data-component))
  (wrapped-text-context-request
   (drakma:http-request ;; "http://localhost:8080/data/organogram-co-2010-10-31-index"
    (concatenate 'string server-url "data/" url-data-component)
    :method :put
    :content content-data-pathname
    ;; (open #p"/home/patrick/dev/sbcl-4store/organogram-co-2010-10-31-index.rdf" 
    ;;       :element-type '(unsigned-byte 8))
    :content-type "application/rdf+xml" :content-length t)))

(defun sparql-server-status (&optional (server-url *4store-base-url*))
  "Return the status of the 4store sparql server from lisp. 
`drakma:http-request' returns multiple values, nth-value 1 is the numerical http status.
 \(sparql-server-status\)"
  ;; (nth-value 1 (drakma:http-request "http://localhost:8080/status"))
  (nth-value 1 (drakma:http-request (concatenate 'string server-url "status"))))

(defun remove-if-typep (type list)
  "A shorthand way to remove an unwanted type of data from a list."
  (remove-if #'(lambda (x) (typep x type)) list))

(defun select-rdfs-classes (&optional (server-url *4store-base-url*))
  "Select all RDFS classses in the knowledgebase. 
Return the multiple values from the query POSTed to the knowledgebase's sparql
http end-point.
nth-value 0 is the body of the response in the sparql query results XML format."
  (wrapped-text-context-request
   (drakma:http-request (concatenate 'string server-url "sparql/")
                        :method :post  
                        :parameters `(("query" . ,(gethash :rdfs-class-select *4store-query-cache*))))))


(defun construct-persons (&optional (server-url *4store-base-url*))
  "Select the FOAF (http://www.foaf-project.org/) Person instances with FOAF
names, and all other triples having the Person as the subject.
Return the multiple values from the query POSTed to the knowledgebase's sparql
http end-point.
nth-value 0 is a graph constructed from the selected triples in RDF/XML format."
  (wrapped-text-context-request
   (drakma:http-request (concatenate 'string server-url "sparql/")
                        :method :post  
                        :parameters `(("query" . ,(gethash :foaf-person-construct *4store-query-cache*))))))

(defun map-uris-to-strings (list)
  "Return a copy of the given list with top-level URIs rendered as strings."
  (mapcar #'(lambda (x)
	    (if (puri:uri-p x)
		(with-output-to-string (stream)
		  (puri:render-uri x stream))
	      x)) list))

(defun extract-persons ()
  "Use cl-rdfxml to parse the RDF/XML of FOAF Persons and related triples.
Return the triples as a list of three-lists with URIs rendered as strings."
  (let ((persons '()))
    (cl-rdfxml:parse-document #'(lambda (s p o)
                                  (push (map-uris-to-strings (list s p o))
                                        persons))
                              (construct-persons))
    ;; not really necessary to reverse, but it's nice to be in the
    ;; same order as the RDF/XML.
    (reverse persons)))

(defun parse-result (xml-result)
  "Use cxml to parse a sparql result XML formatted string into a list structure."
  (cxml:parse xml-result (cxml-xmls:make-xmls-builder)))

;; More meaningful names.
;; (setf (symbol-function 'extract-data) #'sixth)
;; (setf (symbol-function 'extract-rows) #'cdddr)

(defun extract-rdfs-classes ()
  "Return a list of the RDFS class URIs in the knowledgebase. 
Use fare-matcher:letm to pattern-match over a list structure parsed from the
sparql XML format.
Underscores in the pattern are 'don't care' positions. 
The 'uri' variable is the position of interest."
  (mapcar #'(lambda (item) 
              (fare-matcher:letm (list _ _ _ (list _ _ (list _ _ uri)) _) 
                  item 
                uri)) 
          ;; (remove-if-typep 'string  (extract-rows (extract-data (parse-result (select-rdfs-classes)))))))
          (remove-if-typep 'string (cdddr (sixth (parse-result (select-rdfs-classes)))))))
 
;; (extract-rdfs-classes)
;; ("http://purl.org/linked-data/cube#DataSet"
;;  "http://purl.org/linked-data/cube#DataStructureDefinition"
;;  "http://purl.org/linked-data/cube#Observation"
;;  "http://purl.org/net/opmv/ns#Artifact" "http://purl.org/net/opmv/ns#Process"
;;  "http://purl.org/net/opmv/types/google-refine#OperationDescription"
;;  "http://purl.org/net/opmv/types/google-refine#Process"
;;  "http://purl.org/net/opmv/types/google-refine#Project"
;;  "http://rdfs.org/ns/void#Dataset"
;;  "http://reference.data.gov.uk/def/central-government/AssistantParliamentaryCounsel"
;;  "http://reference.data.gov.uk/def/central-government/CivilServicePost"
;;  "http://reference.data.gov.uk/def/central-government/Department"
;;  "http://reference.data.gov.uk/def/central-government/DeputyDirector"
;;  "http://reference.data.gov.uk/def/central-government/DeputyParliamentaryCounsel"
;;  "http://reference.data.gov.uk/def/central-government/Director"
;;  "http://reference.data.gov.uk/def/central-government/DirectorGeneral"
;;  "http://reference.data.gov.uk/def/central-government/ParliamentaryCounsel"
;;  "http://reference.data.gov.uk/def/central-government/PermanentSecretary"
;;  "http://reference.data.gov.uk/def/central-government/PublicBody"
;;  "http://reference.data.gov.uk/def/central-government/SeniorAssistantParliamentaryCounsel"
;;  "http://reference.data.gov.uk/def/intervals/CalendarDay"
;;  "http://www.w3.org/2000/01/rdf-schema#Class"
;;  "http://www.w3.org/ns/org#Organization"
;;  "http://www.w3.org/ns/org#OrganizationalUnit"
;;  "http://xmlns.com/foaf/0.1/Person")

;;; ==============================
;;; EOF

(in-package #:4store)  

;;;; Functions specific to foaf functionality

;;;; Requires these packages to be added to :depends-on in 4store.asd
;:puri 
;:cxml
;:cl-rdfxml
;:fare-matcher

(defvar *4store-query-cache* (make-hash-table))

(setf (gethash :rdfs-class-select *4store-query-cache*)
"prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
select distinct ?type 
where { 
    ?x a ?type .
} 
order by ?type")

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

(defun render-parsed-uri-to-string-if (uri-or-string)
  "Ensure URI-OR-STRING is rendered as a string.~%~@
If URI-OR-STRING satisfies `puri:uri-p' render it with `puri:render-uri', else return 
URI-OR-STRING.~%~@
Helper function for `map-uris-to-strings'.~%~@
:EXAMPLE~%
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶"
  (if (puri:uri-p uri-or-string)
      (with-output-to-string (as-rendered)
			     (puri:render-uri uri-or-string as-rendered))
    uri-or-string))

(defun map-uris-to-strings (list)
  "Return a copy of LIST with top-level URIs rendered as strings.~%~@
:EXAMPLE~%
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶"
  (mapcar #'render-parsed-uri-to-string-if list))

(defun foaf-person-construct-extract ()
  "Return extracted triples as list of three-lists with URIs rendered as strings.~%~@
Invokes `cl-rdfxml:parse-document' when parsing RDF/XML of FOAF Persons and
related triples.~%
:EXAMPLE~%
 \(foaf-person-construct-extract\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶"
  (let ((extracted-persons '()))
    (flet ((render-parsed-persons (s p o)
				  (push (map-uris-to-strings (list s p o))
					extracted-persons)))
      (cl-rdfxml:parse-document #'render-parsed-persons  (foaf-person-construct-request)))
    ;; `cl:nreverse' keeps same order as RDF/XML
    (setf extracted-persons (nreverse extracted-persons))))

(defun render-request-as-xmls (xml-result)
  "Parse SPARQL result XML formatted string into a list structure.~%~@
Parsing is as if by `cxml:parse' and `cxml-xmls:make-xmls-builder'.~%~@
:EXAMPLE~%
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶"
  (cxml:parse xml-result (cxml-xmls:make-xmls-builder)))

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
  "Match URIs in ITEM. 
ITEM is a list structure parsed from the SPARQL XML format.~%~@
URI are identified by pattern matching with `fare-matcher:letm'.~%~@
:EXAMPLE~%
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶"
  (mapcar #'rdfs-class-uri-match filtered-data))

(defun rdfs-class-select-extract ()
  "Return a list of RDFS class URIs in the knowledgebase.~%~@
:EXAMPLE~%
 \(rdfs-class-select-extract\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶"
  (rdfs-class-uri-match-map
   (rdfs-class-select-string-filter    
    (rdfs-class-select-row-data-extract
     (rdfs-class-select-render-as-xmls 
      (rdfs-class-select-request))))))

(defun remove-if-typep (type list)
  "Convenience function to remove unwanted data of TYPE from LIST.
:EXAMPLE~%~@
 \(remove-if-typep 'string \(list \"a\" 8 \"b\" :keyword\)\)
:SEE-ALSO `<XREF>'.~%▶▶▶"
  (remove-if #'(lambda (x) (typep x type)) list))


(defun foaf-person-construct-request (&key (server-url *4store-base-url*))
  "Select FOAF \(http://www.foaf-project.org/\) `Person` instances with FOAF
names, and all other triples having `Person` as subject.~%~@
Return multiple values from query POSTed to knowledgebase's SPARQL http end-point.~%~@
nth-value 0 is a graph constructed from selected triples in RDF/XML format.~%~@
:EXAMPLE~%
 \(foaf-person-construct-request\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶"
  (sparql-query server-url (gethash :foaf-person-construct *4store-query-cache*) :method :post))

(defun rdfs-class-select-request (&key (server-url *4store-base-url*))
  "Select all RDFS classses in knowledgebase.~%~@
Return multiple values of query POSTed to knowledgebase's SPARQL HTTP end-point.~%~@
nth-value 0 is the body of the response in SPARQL query results XML format.~%~@
:EXAMPLE~%
 \(rdfs-class-select-request\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶"
  (sparql-query server-url (gethash :rdfs-class-select *4store-query-cache*) :method :post))


;; From mon-key:
;;
;; Workspace.lisp originally set the symbol-function of `extract-data' and
;; `extract-rows' to `cl:sixth' and `cl:cdddr' respectively, and indicated with
;; the comment that these were: "More meaningful names"...
;; However, while this may be true, setf of symbol-function isn't my style :) 
;; Maybe these becomes a defgeneric at some point :)
;;
;; (setf (symbol-function 'extract-data) #'sixth)
;; (setf (symbol-function 'extract-rows) #'cdddr)

(defun row-extract (row-container)
  (cdddr row-container))

(defun data-extract (data-container)
  (sixth data-container))


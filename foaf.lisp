(in-package #:4store)  

;;;; Functions specific to foaf functionality

;;;; Requires these packages to be added to :depends-on in 4store.asd
;:puri 
;:cxml
;:cl-rdfxml
;:fare-matcher

(defun map-uris-to-strings (list)
  (mapcar #'render-parsed-uri-to-string-if list))

(defun foaf-person-construct-extract ()
  (let ((extracted-persons '()))
    (flet ((render-parsed-persons (s p o)
             (push (map-uris-to-strings (list s p o))
                   extracted-persons)))
      (cl-rdfxml:parse-document #'render-parsed-persons  (foaf-person-construct-request)))
    ;; `cl:nreverse' keeps same order as RDF/XML
    (setf extracted-persons (nreverse extracted-persons))))
(defun render-request-as-xmls (xml-result)
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
  (mapcar #'rdfs-class-uri-match filtered-data))

(defun rdfs-class-select-extract ()
  (rdfs-class-uri-match-map
   (rdfs-class-select-string-filter    
    (rdfs-class-select-row-data-extract
     (rdfs-class-select-render-as-xmls 
      (rdfs-class-select-request))))))

(defun remove-if-typep (type list)
  (remove-if #'(lambda (x) (typep x type)) list))

(fundoc 'render-parsed-uri-to-string-if
        "Ensure URI-OR-STRING is rendered as a string.~%~@
If URI-OR-STRING satisfies `puri:uri-p' render it with `puri:render-uri', else return 
URI-OR-STRING.~%~@
Helper function for `map-uris-to-strings'.~%~@
:EXAMPLE~%
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'map-uris-to-strings
        "Return a copy of LIST with top-level URIs rendered as strings.~%~@
:EXAMPLE~%
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'remove-if-typep
  "Convenience function to remove unwanted data of TYPE from LIST.
:EXAMPLE~%~@
 \(remove-if-typep 'string \(list \"a\" 8 \"b\" :keyword\)\)
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'render-request-as-xmls
  "Parse SPARQL result XML formatted string into a list structure.~%~@
Parsing is as if by `cxml:parse' and `cxml-xmls:make-xmls-builder'.~%~@
:EXAMPLE~%
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'foaf-person-construct-request
        "Select FOAF \(http://www.foaf-project.org/\) `Person` instances with FOAF
names, and all other triples having `Person` as subject.~%~@
Return multiple values from query POSTed to knowledgebase's SPARQL http end-point.~%~@
nth-value 0 is a graph constructed from selected triples in RDF/XML format.~%~@
:EXAMPLE~%
 \(foaf-person-construct-request\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'foaf-person-construct-extract
        "Return extracted triples as list of three-lists with URIs rendered as strings.~%~@
Invokes `cl-rdfxml:parse-document' when parsing RDF/XML of FOAF Persons and
related triples.~%
:EXAMPLE~%
 \(foaf-person-construct-extract\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'rdfs-class-select-request
        "Select all RDFS classses in knowledgebase.~%~@
Return multiple values of query POSTed to knowledgebase's SPARQL HTTP end-point.~%~@
nth-value 0 is the body of the response in SPARQL query results XML format.~%~@
:EXAMPLE~%
 \(rdfs-class-select-request\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'rdfs-class-uri-match
        "Match URIs in ITEM. 
ITEM is a list structure parsed from the SPARQL XML format.~%~@
URI are identified by pattern matching with `fare-matcher:letm'.~%~@
:EXAMPLE~%
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'rdfs-class-select-extract
        "Return a list of RDFS class URIs in the knowledgebase.~%~@
:EXAMPLE~%
 \(rdfs-class-select-extract\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")



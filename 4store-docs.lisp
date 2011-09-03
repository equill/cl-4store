;;; :FILE-CREATED <Timestamp: #{2011-09-02T17:15:36-04:00Z}#{11355} - by MON>
;;; :FILE sbcl-4store-FORK/4store-doc.lisp
;;; ==============================


(in-package #:4store)


;;; ==============================
;;; :DOCUMENTATION
;;; ==============================

(fundoc 'sparql-server-put-data-request
        "Perfrom an HTTP put request with the data contained of CONTENT-DATA-PATHNAME.~%~@
CONTENT-DATA-PATHNAME should name an existing file.~%~@
URL-DATA-COMPONENT is identifies the releative URL component of the content to put.~%~@
Keyword SERVER-URL is the base url pathname of sparql server.~%~@
Default is value of `*4store-base-url*'.~%~@
The full destination of the put request is the concatatenated return value of
`render-url-components' such that the put request and has the form:~%
 <SERVER-URL>data/<URL-DATA-COMPONENT>~%~@
:EXAMPLE~%
 \(let \(\(content/component \"organogram-co-2010-10-31-index\"\)\)
   \(sparql-server-put-data-request
    \(make-pathname :name content/component
                   :type \"rdf\"
                   :defaults *default-pathname-defaults*\)
    content/component
    :server-url \"http://localhost:8080/\"\)\)~%~@
:NOTE equivalalent of: 
 shell> curl -v -T organogram-co-2010-10-31-index.rdf \\
       'http://localhost:8080/data/organogram-co-2010-10-31-index'~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'sparql-server-status-request
        "Return the status of the 4store sparql server at SERVER-URL ~%~@
Return as if by `drakma:http-request's  multiple values.~%~@
nth-value 1 is the numerical http status.~%~@
:EXAMPLE~%
 \(sparql-server-status-request\)
:SEE-ALSO `<XREF>'.~%▶▶▶")

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
  "Parse sparql result XML formatted string into a list structure.~%~@
Parsing is as if by `cxml:parse'  and `cxml-xmls:make-xmls-builder'.~%~@
:EXAMPLE~%
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'foaf-person-construct-request
        "Select FOAF \(http://www.foaf-project.org/\) `Person` instances with FOAF
names, and all other triples having `Person` as subject.~%~@
Return multiple values from query POSTed to knowledgebase's sparql http end-point.~%~@
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
Return multiple values of query POSTed to knowledgebase's sparql http end-point.~%~@
nth-value 0 is the body of the response in sparql query results XML format.~%~@
:EXAMPLE~%
 \(rdfs-class-select-request\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'rdfs-class-uri-match
        "Match URIs in ITEM. 
ITEM is a list structure parsed from the sparql XML format.~%~@
URI are identified by pattern matching with `fare-matcher:letm'.~%~@
:EXAMPLE~%
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'rdfs-class-select-extract
        "Return a list of RDFS class URIs in the knowledgebase.~%~@
:EXAMPLE~%
 \(rdfs-class-select-extract\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'render-url-components
"Return COMPONENTS rendered as URL string for use with `drakma:http-request'.~%~@
:EXAMPLE~%
 \(render-url-components \"http://localhost:8080/\"
                        \"data/\" 
                        \"organogram-co-2010-10-31-index\"\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")


;;; ==============================
;;; EOF

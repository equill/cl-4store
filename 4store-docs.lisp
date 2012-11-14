(in-package #:4store)


;;; ==============================
;;; :DOCUMENTATION
;;; ==============================

(fundoc 'sparql-server-put-data-request
        "Perform an HTTP put request with the data contained of CONTENT-DATA-PATHNAME.~%~@
CONTENT-DATA-PATHNAME should name an existing file.~%~@
URL-DATA-COMPONENT is identifies the relative URL component of the content to put.~%~@
Keyword SERVER-URL is the base url pathname of the SPARQL server.~%~@
Default is value of `*4store-base-url*'.~%~@
The full destination of the put request is the concatenation of the supplied URL components
 such that the put request has the form:~%
 <SERVER-URL>data/<URL-DATA-COMPONENT>~%~@
:EXAMPLE~%
 \(let \(\(content/component \"organogram-co-2010-10-31-index\"\)\)
   \(sparql-server-put-data-request
    \(make-pathname :name content/component
                   :type \"rdf\"
                   :defaults *default-pathname-defaults*\)
    content/component
    :server-url \"http://localhost:8080/\"\)\)~%~@
:NOTE equivalent of: 
 shell> curl -v -T organogram-co-2010-10-31-index.rdf \\
       'http://localhost:8080/data/organogram-co-2010-10-31-index'~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'sparql-server-status-request
        "Return the status of the 4store sparql server at SERVER-URL ~%~@
Return as if by `drakma:http-request's multiple values.~%~@
nth-value 1 is the numerical http status.~%~@
:EXAMPLE~%
 \(sparql-server-status-request\)
:SEE-ALSO `<XREF>'.~%▶▶▶")

;;; ==============================
;;; EOF

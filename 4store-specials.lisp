(in-package #:4store)


;; Tell drakma to treat these additional types as text rather than
;; binary.
(defvar *4store-text-content-types* 
  (list* '("application". "sparql-results+xml")
         '("text" . "tab-separated-values")
         '("application" . "sparql-results+json")
         '("application" . "rdf+xml")
         '("text" . "plain")
         drakma:*text-content-types*))

(defvar *4store-base-url* "http://localhost:8080/")


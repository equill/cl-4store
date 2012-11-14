(in-package #:4store)


;; Tell drakma to treat these additional types as text rather than
;; binary.
(defvar *4store-text-content-types* 
  (list* '("application". "sparql-results+xml")
	 '("text" . "tab-separated-values")
	 '("application" . "sparql-results+json")
         '("application" . "rdf+xml")
         drakma:*text-content-types*))

(defvar *4store-base-url* "http://localhost:8080/")

(defvar *4store-query-cache* (make-hash-table))

(setf (gethash :rdfs-class-select *4store-query-cache*)
"prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
select distinct ?type 
where { 
    ?x a ?type .
} 
order by ?type")


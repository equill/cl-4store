;;; :FILE-CREATED <Timestamp: #{2011-09-02T17:08:12-04:00Z}#{11355} - by MON>
;;; :FILE sbcl-4store-FORK/4store-specials.lisp
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



;;; ==============================
;;; EOF

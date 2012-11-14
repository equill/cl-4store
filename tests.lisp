(in-package :4store-tests)

(defparameter *base-uri* "http://localhost:8080/")
(defparameter *graph-name* "demo")

(fiveam:def-suite 4store-tests)
(fiveam:in-suite 4store-tests)

;; Can we get a sane status reply from the server?
(fiveam:test (status)
	     (fiveam:is
	      (= 200 (4store:sparql-server-status-request *base-uri*))))

;; Can we store a set of triples?
(fiveam:test (insert-triples)
	     (fiveam:is
	      (cl-ppcre:scan "200 added successfully"
		     (4store:insert-triples
		      *base-uri*
		      *graph-name*
		      '(("<ox-subjects:foo>" "<ox-predicates:bar>" "plaintext")
			("<ox-subjects:first-hash>" "<rdfs:label>" "Comment on the primacy of labels")
			("<ox-subjects:first-hash>" "<rdfs:creator>" "Name of the creator"))))))

;; Do we get the correct set of triples back?
(fiveam:test (get-triples)
	     (fiveam:is
	      (equalp (4store:get-triples-list *base-uri*)
		     "<?xml version=\"1.0\"?>
<sparql xmlns=\"http://www.w3.org/2005/sparql-results#\">
  <head>
    <variable name=\"subject\"/>
    <variable name=\"predicate\"/>
    <variable name=\"object\"/>
  </head>
  <results>
    <result>
      <binding name=\"subject\"><uri>ox-subjects:foo</uri></binding>
      <binding name=\"predicate\"><uri>ox-predicates:bar</uri></binding>
      <binding name=\"object\"><literal>plaintext</literal></binding>
    </result>
    <result>
      <binding name=\"subject\"><uri>ox-subjects:first-hash</uri></binding>
      <binding name=\"predicate\"><uri>rdfs:label</uri></binding>
      <binding name=\"object\"><literal>Comment on the primacy of labels</literal></binding>
    </result>
    <result>
      <binding name=\"subject\"><uri>ox-subjects:first-hash</uri></binding>
      <binding name=\"predicate\"><uri>rdfs:creator</uri></binding>
      <binding name=\"object\"><literal>Name of the creator</literal></binding>
    </result>
  </results>
</sparql>
")))

;; Can we nuke the graph?
(fiveam:test (delete-graph)
	     (fiveam:is (equal (4store:delete-graph *base-uri* *graph-name*)
			       "200 deleted successfully
This is a 4store SPARQL server v1.1.5-27-gc3d8593
")))

;; Is the graph empty?
(fiveam:test (empty-graph)
	     (fiveam:is (equal (4store:get-triples-list *base-uri*)
			       "<?xml version=\"1.0\"?>
<sparql xmlns=\"http://www.w3.org/2005/sparql-results#\">
  <head>
    <variable name=\"subject\"/>
    <variable name=\"predicate\"/>
    <variable name=\"object\"/>
  </head>
  <results>
  </results>
</sparql>
")))


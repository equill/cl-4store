(in-package :4store-tests)

(defparameter *base-uri* "http://localhost:8080/")
(defparameter *graph-name* "<ox:demo>")

(defparameter *empty-graph-text*
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
")

(defparameter *initial-triples*
  '(("<ox-subjects:foo>" "<ox-predicates:bar>" "plaintext")
    ("<ox-subjects:first-hash>" "<rdfs:label>" "Comment on the primacy of labels")
    ("<ox-subjects:first-hash>" "<rdfs:creator>" "Name of the creator")))

(defparameter *initial-text*
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
")

(defparameter *triple-to-delete*
  '("<ox-subjects:first-hash>" "<rdfs:creator>" "Name of the creator"))

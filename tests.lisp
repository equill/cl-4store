(in-package :4store-tests)

(fiveam:def-suite 4store-tests)
(fiveam:in-suite 4store-tests)

;; Can we get a sane status reply from the server?
(fiveam:test (status)
	     (fiveam:is
	      (= 200 (4store:sparql-server-status-request *server-url*))))

;; Can we store a set of triples?
(fiveam:test (insert-triples)
             (fiveam:is
               (cl-ppcre:scan "200 added successfully"
                              (4store:insert-triples *server-url* *graph-name* *initial-triples*))))

;; Do we get the correct set of triples back?
(fiveam:test (get-triples)
             (fiveam:is
               (equalp *initial-text*
                       (4store:get-triples-list *server-url* *graph-name*))))

;; Can we nuke the graph?
(fiveam:test (delete-graph)
             (fiveam:is (equal (4store:delete-graph *server-url* *graph-name*)
                               "200 deleted successfully
This is a 4store SPARQL server v1.1.5-27-gc3d8593
")))

;; Is the graph empty?
(fiveam:test (empty-graph)
             (fiveam:is 
               (equal *empty-graph-text*
                      (4store:get-triples-list *server-url* *graph-name*))))

;; Can we delete one triple?
(defun test-delete ()
  (4store:delete-triple
   *server-url*
   *graph-name*
   (first *triple-to-delete*)
   (second *triple-to-delete*)
   (third *triple-to-delete*)))

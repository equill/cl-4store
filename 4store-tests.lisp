(in-package :4store-tests)

(fiveam:def-suite 4store-tests)
(fiveam:in-suite 4store-tests)

;; Can we get a sane status reply from the server?
(fiveam:test (status)
             (fiveam:is
               (= 200 (4store:sparql-server-status-request *server-url*))))

;; Can we store a set of triples?
;; Yes, this looks a little convoluted, but I'd rather rely on the HTTP code
;; than the response text
(fiveam:test (main)

             ;; Are we starting with a clean slate?
             (fiveam:is (null (4store:get-triples-list *server-url* *graph*)))

             (fiveam:is
               (equal 200
                      (multiple-value-bind (text-response numeric-response)
                        (4store:insert-triples
                         *server-url*
                         *graph*
                         '(("<http://ox.electronic-quill.net/4store#foo>"
                            "<http://ox.electronic-quill.net/4store#bar>"
                            "'''plaintext'''")
                           ("<http://ox.electronic-quill.net/4store#first-hash>"
                            "<http://ox.electronic-quill.net/4store#label>"
                            "'''Comment on the primacy of labels'''")
                           ("<http://ox.electronic-quill.net/4store#first-hash>"
                            "<http://ox.electronic-quill.net/4store#creator>"
                            "'''Name of the creator'''")))
                        (declare (ignore text-response))
                        numeric-response)))

             ;; Do we get the correct set of triples back?
             (fiveam:is
               (equalp '(("<http://ox.electronic-quill.net/4store#foo>"
                          "<http://ox.electronic-quill.net/4store#bar>"
                          "\"plaintext\"")
                         ("<http://ox.electronic-quill.net/4store#first-hash>"
                          "<http://ox.electronic-quill.net/4store#creator>"
                          "\"Name of the creator\"")
                         ("<http://ox.electronic-quill.net/4store#first-hash>"
                          "<http://ox.electronic-quill.net/4store#label>"
                          "\"Comment on the primacy of labels\""))
                       (4store:get-triples-list *server-url* *graph*)))

             ;; Can we delete one triple?
             (fiveam:is
               (equal 200
                      (multiple-value-bind (text-response numeric-response)
                        (4store:delete-triples
                         *server-url*
                         *graph*
                         '(("<http://ox.electronic-quill.net/4store#first-hash>"
                            "<http://ox.electronic-quill.net/4store#creator>"
                            "'''Name of the creator'''")))
                        (declare (ignore text-response))
                        numeric-response)))

             ;; What's left after we've deleted that?
             (fiveam:is
               (equal '(("<http://ox.electronic-quill.net/4store#foo>"
                         "<http://ox.electronic-quill.net/4store#bar>"
                         "\"plaintext\"")
                        ("<http://ox.electronic-quill.net/4store#first-hash>"
                         "<http://ox.electronic-quill.net/4store#label>"
                         "\"Comment on the primacy of labels\""))
                      (4store:get-triples-list *server-url* *graph*)))

             ;;;; Can we delete them anyway?
             (fiveam:is
               (equal 200
                      (multiple-value-bind (text-response numeric-response)
                        (4store:delete-all-triples *server-url* *graph*)
                        (declare (ignore text-response))
                        numeric-response)))

             ;; Is the graph empty?
             (fiveam:is (null (4store:get-triples-list *server-url* *graph*)))

             ;; Can we nuke the graph?
             ;; FIXME: currently fails despite apparent success
             (fiveam:is (cl-ppcre:all-matches "200 deleted successfully"
                                              (4store:delete-graph *server-url* *graph*))))

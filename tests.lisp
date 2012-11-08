(in-package :4store-tests)

(defun test-insert-triple ()
  (4store::insert-triple
   "http://localhost:8080/"
   "demo"
   "<http://www.electronic-quill.net/foo/s>"
   "<http://www.electronic-quill.net/bar/p>"
   "<http://www.electronic-quill.net/baz/o>"))
;;; :FILE-CREATED <Timestamp: #{2011-09-02T17:09:55-04:00Z}#{11355} - by MON>
;;; :FILE 4store-macros.lisp
;;; ==============================

(in-package #:4store)

(defmacro wrapped-text-context-request (&rest body)
  `(let ((drakma:*text-content-types* *4store-text-content-types*))
     ,@body))

;;; ==============================
;;; EOF

(in-package #:4store)

(defmacro wrapped-text-context-request (&rest body)
  `(let ((drakma:*text-content-types* *4store-text-content-types*))
     ,@body))

;;; ==============================
;;; EOF

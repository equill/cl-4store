;;; :FILE-CREATED <Timestamp: #{2011-09-02T02:33:13-04:00Z}#{11355} - by MON>
;;; :FILE sbcl-4store-GIT/package.lisp
;;; ==============================


(defpackage #:4store (:use #:common-lisp 
                           #:drakma 
                           #:fare-matcher
                           #:cl-rdfxml)
            (:import-from #:puri
                          #:render-uri
                          #:uri-p))

;; ,----
;; | (use-package 'drakma)
;; | (use-package 'fare-matcher)
;; | ;(use-package 'cl-utilities)
;; | (use-package 'cl-rdfxml)
;; | (import 'puri::render-uri)
;; | (import 'puri::uri-p)
;; | ;(import 'hunchentoot::url-encode)
;; | ;(import 'hunchentoot::url-decode)
;; `----


;;; ==============================
;;; EOF

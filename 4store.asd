;;; :FILE-CREATED <Timestamp: #{2011-09-02T02:32:46-04:00Z}#{11355} - by MON>
;;; :FILE /sbcl-4store-GIT/4store.asd
;;; ==============================

(defpackage #:4store-build-system (:use :common-lisp :asdf))

(in-package #:4store-build-system)

(defsystem :4store
  :author  "MON KEY"
  :maintainer "MON KEY"
  :license "MIT" 
  :description "4store"
  :version "1.0.0"
  :depends-on (:puri 
               :drakma 
               :cxml
               :fare-matcher
               :cl-rdfxml)
  :serial t
  :components ((:file "package")
               (:file "4store")))



;;; ==============================
;;; EOF

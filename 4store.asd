;;; :FILE-CREATED <Timestamp: #{2011-09-02T02:32:46-04:00Z}#{11355} - by MON>
;;; :FILE sbcl-4store-FORK/4store.asd
;;; ==============================

(defpackage #:4store-build-system (:use :common-lisp :asdf))

(in-package #:4store-build-system)

(defsystem :4store
  ;; :author  "Patrick D. Logan"
  ;; :maintainer "MON KEY"
  :license "BSD" 
  :description "4store"
  :version "0.0.1"
  :depends-on (:puri 
               :drakma 
               :cxml
               :cl-rdfxml
               :fare-matcher)
  :serial t
  :components ((:file "package")
               (:file "4store-specials")
               (:file "4store-macros")
               (:file "4store-utils")
               (:file "4store")
               (:file "4store-docs"))

;;; ==============================
;;; EOF

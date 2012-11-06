;;; ==============================

(defpackage #:4store-build-system (:use :common-lisp :asdf))

(in-package #:4store-build-system)

(defsystem :4store
  ;; :author  "Patrick D. Logan"
  ;; :maintainer "James Fleming"
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
               (:file "4store-docs")))

;;; ==============================
;;; EOF

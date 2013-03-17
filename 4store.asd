(defsystem :4store
           :author  "Patrick D. Logan"
           :maintainer "James Fleming"
           :license "BSD" 
           :description "4store"
           :version "0.0.1"
           :depends-on (:drakma
                         :split-sequence)
           :serial t
           :components ((:file "defpackage")
                        (:file "4store-specials")
                        (:file "4store")))

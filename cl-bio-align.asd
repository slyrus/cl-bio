
(asdf:defsystem #:cl-bio-align
  :name "cl-bio-align"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :version "0.2.6"
  :licence "BSD"
  :depends-on (cl-bio)
  :components
  ((:module :align
            :components
            ((:cl-source-file "defpackage")
             (:cl-source-file "align" :depends-on ("defpackage"))
             (:module :matrix
                      :components
                      ((:static-file "blosum62" :pathname #p"blosum62.bla")))))))


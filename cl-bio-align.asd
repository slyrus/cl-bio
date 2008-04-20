
(asdf:defsystem #:cl-bio-align
  :name "cl-bio-align"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :version #.(with-open-file
                 (vers (merge-pathnames "version.lisp-expr" *load-truename*))
               (read vers))
  :licence "BSD"
  :depends-on (cl-bio ch-asdf)
  :components
  ((:module :align
            :components
            ((:cl-source-file "defpackage")
             (:cl-source-file "align" :depends-on ("defpackage"))
             (:module :matrix
                      :components
                      ((:static-file "blosum62" :pathname #p"blosum62.bla")))))))


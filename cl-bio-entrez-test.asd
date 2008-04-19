
(asdf:defsystem #:cl-bio-entrez-test
  :name "cl-bio-entrez-test"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :version #.(with-open-file
                 (vers (merge-pathnames "version.lisp-expr" *load-truename*))
               (read vers))
  :licence "BSD"
  :description "Tests for cl-bio-entrez"
  :depends-on (cl-bio-entrez)
  :components
  ((:module :entrez
            :components
            ((:module :test
                      :components
                      ((:cl-source-file "defpackage")
                       (:cl-source-file "cl-bio-entrez-test" :depends-on ("defpackage"))))))))

